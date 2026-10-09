# Option: a browser-only portal

A proposal for discussion, written on 10 October 2026 alongside [REVIEW.md](REVIEW.md). Nothing has been decided or built. The size and layout figures below were measured on the AusTraits 7.0.0 portal build; the effort figures are estimates.

## The two options

**Current: a Shiny server.** All filtering, table paging, plotting and download assembly happen in R on one AWS machine. DuckDB queries the Parquet files on that machine. Open-source Shiny Server runs one R process per app, so every user's request waits behind everyone else's.

**Proposed: a static site that queries the data in the browser.** The Parquet files and a web page sit on static hosting (Cloudflare or GitHub Pages, for example). The page loads [DuckDB-WASM](https://duckdb.org/docs/api/wasm/overview), DuckDB compiled to run in the browser. It queries the Parquet files over HTTP, fetching only the byte ranges it needs. There is no server-side code: each visitor's own browser does the work. Taxon and trait profiles become static pages generated in R when the data is built, the way the APD site works.

## Comparison

| | Current (Shiny server) | Proposed (browser-only) |
|---|---|---|
| Where the work happens | One R process, shared by all users | Each user's browser |
| Many users at once | Users queue; one download freezes everyone for about 10 s (REVIEW.md) | No shared bottleneck |
| Hosting | An EC2 instance, Shiny Server, SSH deploys, and restarts that drop live sessions | Static files; close to free; deploy by uploading files; no sessions to drop |
| First visit | Fast: the page is small and the server does the work | About 6 MB (compressed) to download the DuckDB engine, cached afterwards, plus the data each query touches |
| Each filter after that | One server round trip, plus the server-side work | Local, plus fetching any data not yet downloaded |
| Taxon and trait profiles | Generated on request in R (Taxon View is currently broken, B5) | Pre-built at data-release time: about 33,000 taxon pages and 530 trait pages, which can be linked, indexed by search engines, and cached |
| Downloads | Built in R (bibliography, citations, zip) | Built in the browser from pre-built per-dataset citation and BibTeX snippets. Very large selections should point to the Zenodo release instead: the full raw table is 6.9 GB as CSV, because every row repeats long method and description text. The current portal will also attempt that download if asked. |
| Data versioning | The server's data is not in git (REVIEW.md B5) | Data files are published per release next to the site; the page and its data are deployed together |
| Usage metrics | Logged from R to Supabase | Logged from the browser to Supabase. The key then becomes public, so Supabase's row-level security must allow inserts only; today that key can also read the whole telemetry table. |
| Skills needed to maintain it | R and Shiny, which the team has | JavaScript or TypeScript for the front end; R stays for the data build |
| Fit with austraits.org | Embedded in an iframe | Can be embedded the same way, or become part of the site itself |

## What the data layout means for the browser option

The current Parquet files are laid out for a server, which reads them from local disk. Over HTTP, the layout decides how much each visitor downloads.

- `austraits-display.parquet` has only 2 row groups of about 1M rows each, sorted by family, taxon and trait. A browser asking for one trait cannot skip anything, so it would fetch whole columns for all 1.8M rows. Splitting the file by `trait_name` fixes this: there are 530 traits, the median trait has 281 rows, and the largest has 113,000. A trait query would then fetch one small file.
- In `austraits-species-averages.parquet`, 27 MB of its 49 MB is the `source_primary_citation` column: the full citation text repeated on every row. Storing dataset keys only, and joining to a sources table when needed, would shrink the file roughly by half. That helps the current server too.
- `row_id` is the largest single column in the display and data files (8.4 MB each). The browser version doesn't need it.

These are changes to `prepare_data_for_portal()`, the R data build, which stays in R under either option.

## Effort

Claude can write the code quickly. The bottleneck is checking it and deciding things, not typing.

- **Prototype: an afternoon.** A static page with DuckDB-WASM, the trait-split Parquet files, filters, a paged table and CSV download. This answers the open questions below with real numbers.
- **Matching the current portal: a few days of Claude's coding**, spread over one to two weeks of elapsed time. That covers species averages, custom filters, the georeferenced filter, the citations tab, the download bundle, links that carry filter settings, the pre-built profile pages, telemetry, hosting setup, and a test that checks its results against the current portal on the same filters.
- **The people-time is the real cost:**
  - testing in different browsers, especially phones;
  - reviewing the design and wording;
  - choosing hosting and setting up the security rules on the telemetry table;
  - deciding who owns a JavaScript code base in an R team.

My earlier estimate of "months" assumed a developer writing all of it by hand. With Claude writing it, that figure is too high. But a few hours only covers the prototype, not a portal ready to replace the current one.

## Open questions a prototype would answer

1. How long does a first visit take on a phone over 4G, including the 6 MB engine plus the data for one large trait?
2. Does DuckDB-WASM stay within mobile Safari's memory limits for the largest trait (113,000 raw rows) and for an unfiltered species-averages query?
3. Is a browser-built download (CSV plus citations plus BibTeX) fast enough for a typical selection, and where should the cutoff be for sending users to Zenodo instead?

## Recommendation

1. **Fix the production bugs now regardless.** Steps O1–O3 of REVIEW.md are small, and B1 is hiding 12% of taxa from users today. The trait-split Parquet layout and the citation-column cleanup belong in the data build under either option, so that work isn't wasted.
2. **Build the browser prototype** against the 7.0.0 data and answer the three questions above.
3. **Decide between the two options** using those answers and the `shinyloadtest` numbers for the current portal. If the prototype is fast on a phone, the browser-only portal removes most of the operational problems in REVIEW.md (the shared R process, server memory, session-dropping deploys, unversioned server data) rather than fixing them one by one.
