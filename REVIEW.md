# austraits.portal review — October 2026

A code review and performance profile of `master` at commit `e9ac0e6` (10 October 2026). Fixes are on branch `fix/review-2026-10`; the status column of the [order of work](#proposed-order-of-work) table says what each step changed and what is still open. Line references are to that commit and will drift as the code changes; when an item is fixed, mark it done in the [order of work](#proposed-order-of-work) table rather than deleting it, and retire this file once the table is empty.

Measurements were taken on a laptop (macOS, R 4.6.1, duckdb 1.5.2, arrow 25.0.0, shiny 1.13.0) against the AusTraits 7.0.0 portal build in `inst/extdata/austraits/austraits-7.0.0-full/` (1,798,215 rows). Absolute times on the AWS server will differ; the ratios should hold. Each bug below was reproduced unless it says otherwise. The code to reproduce the key numbers is at the [end](#reproducing-the-measurements).

## Summary

The architecture is sound: DuckDB queries over Parquet, counting rows before collecting them, precomputed dropdown values, and reactives gated on the active tab. Most of the problems trace back to two things:

1. **DuckDB reads the data through Arrow** (`duckdb_register_arrow()`), and on that path `%in%` with two or more values fails. The filters were rewritten as regular expressions to get around this, and those regexes break on 12% of taxon names.
2. **Several code paths `collect()` a whole table into R** when they need a few rows or a single column. The most expensive is the download handler, which loads the entire 1.8M-row table for every download.

Open-source Shiny Server runs one R process per app, so every slow or blocking call stalls all users at once, not only the person who triggered it.

## Bugs

### B1. The taxon filter returns nothing for 3,906 taxa

`apply_filters()` turns the selected names into anchored regexes such as `^(Abutilon sp. Cape Range (A.S.George 1312))$` ([R/fct_filter_application.R:47-67](R/fct_filter_application.R#L47-L67)). Parentheses become regex groups and square brackets become character classes, so the pattern no longer matches the name it came from.

- 3,906 of the 33,370 taxon names (12%) contain `(`, `)`, `[` or `]`. Examples include phrase names like `Abutilon sp. Cape Range (A.S.George 1312)` and dataset-qualified names like `Abrotanella sp. [White_2020]`.
- All 20 of these names that were sampled returned the wrong row count. For example, `(Dockrillia pugioniformis x Dockrillia striolata) x Dockrillia pugioniformis` returns 0 rows instead of 3.
- 49,357 records belong to these taxa and cannot be reached through the taxon filter.
- The failure is silent: the user gets an empty table and no message.

Family, genus and trait-name filters are built the same way. No family or trait name contains a regex metacharacter today; two genus values do (`(Dockrillia` and `Gen.`; see also #113).

**Cause:** the regexes exist because `%in%` fails on the Arrow path (B2). A test asserts the workaround: [tests/testthat/test-filter-parsing.R:158](tests/testthat/test-filter-parsing.R#L158), "apply_filters uses grepl instead of %in% for trait_name to avoid Arrow pushdown error".

**Fix:** O1 (let DuckDB read the Parquet files directly), then use `%in%` for every exact-match filter and rewrite that test. Escaping the regexes would hide the symptom but keep the slower query and leave B2 in place.

### B2. Multi-value custom filters fail, and the `dataset_id` filter over-matches

On Arrow-registered tables, duckdb 1.5.2 rejects any `IN` with two or more values: `Not implemented Error: Arrow table filter pushdown optional: … IN (…)`. A single value works. The deprecated `manifest.json` pins duckdb 1.4.4, but nothing in the repo records which version the AWS server runs, so whether the server fails the same way is unverified.

- Custom filters on controlled-vocabulary columns use `%in%` ([R/fct_filter_application.R:137](R/fct_filter_application.R#L137)). Selecting two values (two life stages, say) throws inside the `tryCatch`, the table is set to empty, and the error is printed only to the server log.
- The `dataset_id` custom filter uses an unanchored regex ([R/fct_filter_application.R:133-135](R/fct_filter_application.R#L133-L135)). 8 of the 410 dataset IDs are substrings of other IDs, so filtering on `Bryant_2021` also returns `Bryant_2021_2` and `Bryant_2021_3`. The match is unanchored because species-average rows hold `; `-joined ID lists. Matching whole list elements fixes both cases, for example with the pattern `(^|; )ID(;|$)` or DuckDB's `list_contains(string_split(dataset_id, '; '), ID)`.
- Free-text custom filters pass the user's text straight into a regex ([R/fct_filter_application.R:140-141](R/fct_filter_application.R#L140-L141)), so typing `(` raises an error. The help text documents `|` as a multi-search operator. To keep that feature, split the input on `|` and escape each part.

### B3. The second "Apply Filters" button drops clicks

The sidebar contains two `actionButton(ns("apply_filters_btn"))` elements with the same ID ([R/mod_filters.R:27](R/mod_filters.R#L27) and [R/mod_filters.R:221](R/mod_filters.R#L221)). Each button keeps its own click counter, and Shiny's client never resends a value equal to the last one it sent (`InputNoResendDecorator` in shiny.js). If a user clicks the top button once and later clicks the bottom button once, both send `1`; the second click is dropped and nothing happens. This was worked out from Shiny 1.13's JavaScript and has not yet been confirmed in a browser.

**Fix:** keep both buttons, because users who have scrolled down the sidebar miss the top one. Give the bottom button its own ID (for example `apply_filters_btn_bottom`) and have the apply handler in `app_server` react to either button: `observeEvent(list(input[["filters-apply_filters_btn"]], input[["filters-apply_filters_btn_bottom"]]), …, ignoreInit = TRUE)`.

### B4. Taxon View ends the session after "Clear Filters"

"Clear Filters" sets `filtered_database(NULL)`. If the user then picks one taxon name and opens Taxon View without pressing Apply, the fallback at [R/mod_taxon_view.R:129-135](R/mod_taxon_view.R#L129-L135) calls `apply_filters(austraits_display, filter_vals)`. It passes the raw `filters()` list instead of the output of `parse_filters()`. In that list `filter_vals$location` is a string, so the call fails with `$ operator is invalid for atomic vectors`. The error happens inside an observer, so Shiny ends the session. Had it not failed, none of the filters would have matched the unparsed list's shape, and the call would have collected the whole unfiltered dataset into R.

**Fix:** delete the fallback. The observer only uses `data` to check for zero rows, and `generate_taxon_text()` already queries the data for the taxon itself.

### B5. Taxon View fails on both data builds in the repo

[R/generate_taxon_text.R:71-78](R/generate_taxon_text.R#L71-L78) formats categorical traits using a `value_range` column. Neither species-averages file has that column: not the committed `austraits-5.0.0-lite/` build, and not a local `austraits-7.0.0-full/` build from February. As a result, `generate_taxon_text("Eucalyptus globulus")` fails with `object 'value_range' not found`. It presumably works on AWS because the server holds a newer build of the data, which is not versioned anywhere.

Two gaps let this through:

- Nothing checks that the data on disk has the columns the code expects. A startup check of each Parquet file's expected columns would fail early with a clear message.
- The only test of `generate_taxon_text()` covers its error path ([tests/testthat/test-text-generation.R:3](tests/testthat/test-text-generation.R#L3)). Run locally against data that breaks Taxon View, all 94 tests pass (22 of them skipped).

A smaller point: the lite folder is named `5.0.0-lite`, but its metadata says v7.0.0.

### B6. The data table deletes text it reads as HTML

The data table is rendered with `escape = FALSE` ([R/mod_data_table.R:106](R/mod_data_table.R#L106)). traits.build writes context descriptions inside double angle brackets, for example `trait scoring method==inferred_from_genus <<Trait value inferred from genus-level taxon description.>>`. The browser reads `<Trait value …>` as an unknown HTML tag and drops it, so the description disappears. 532,439 rows (30%) have text like this in a displayed context-property column. None of the displayed columns needs HTML, and `escape = FALSE` also lets any contributed text inject markup into the page.

**Fix:** `escape = TRUE` (the default).

### B7. The "Searches" usage metric overcounts

[R/app_server.R:285-294](R/app_server.R#L285-L294) logs a `search` telemetry event whenever `filtered_database()` changes. Besides Apply, that happens on the automatic load when a session starts, on every dataset switch, and on every "Load next 100" click. So every page view counts as at least one search, and paging also counts as searching. These numbers feed the COUNTER CSV/JSON export (`Requests_Count`), so reported usage is inflated by an unknown factor.

**Fix:** log `search` only in the Apply and URL-load handlers. If paging is worth tracking, give it a separate event type.

### B8. Taxon profiles are cached on disk with no expiry

`generate_taxon_text_cached` memoises results into `.cache/taxon_text` ([R/generate_taxon_text.R:319-322](R/generate_taxon_text.R#L319-L322)). The cache key covers the taxon name and the function's code, but not the data. `.cache/` is gitignored, and the deploy (`git pull` plus a restart) never clears it. After a data release, users see profiles built from the old data until someone deletes the folder by hand.

**Fix:** put the data version (from `metadata.rds`) in the cache path, for example `.cache/taxon_text/<version>/`.

## Performance

| Path | Now | Alternative | What a user would notice |
|---|---|---|---|
| Download: [R/srv_download.R:57-65](R/srv_download.R#L57-L65) loads the whole 1.8M × 69 data table into R, then keeps the filtered `row_id`s | 9.9 s for a 33k-row download (leaf_mass_per_area, raw); R heap peaks at 1.2 GB, and the process grew 6–12 GB on macOS | filter in DuckDB: 0.3 s as an SQL `IN` subquery, 0.7 s via dbplyr `semi_join()`; about +0.3 GB | About 10 s less on every download, whatever its size: a 50-row download costs the same 10 s today. While a download runs, every other user is frozen. |
| `RefManageR::ReadBib(sources.bib)` on every download ([R/generate_usage_text.R:79-83](R/generate_usage_text.R#L79-L83)) | 1.5 s | read it once at startup | A further 1.5 s off each download |
| Trait View without exactly one trait: collects the full filtered query ([R/mod_trait_view.R:69](R/mod_trait_view.R#L69)), then collects it again for the plot ([R/mod_trait_view.R:173](R/mod_trait_view.R#L173)) | 4.0 s and +1 GB on unfiltered raw data, only to show "Please select a single trait" | `req()` exactly one trait before collecting, and collect once | Opening the tab becomes instant |
| First "Load next 100" on a large result ([R/app_server.R:313-321](R/app_server.R#L313-L321)): collects the entire result and keeps it in memory for that session | 4.0 s and +1 GB on unfiltered raw data, per session | query each page with `LIMIT`/`OFFSET` | The first page-load becomes instant, and server memory no longer grows with each session |
| Georeferenced filter: lat/lon are stored as strings, checked by regex, then the whole result is collected into R before counting ([R/app_server.R:95-98](R/app_server.R#L95-L98), [R/fct_filter_application.R:88-113](R/fct_filter_application.R#L88-L113)) | 1.1 s, 381k rows, +621 MB | store lat/lon as `DOUBLE` at prep time and keep the query lazy | About 1 s off each georeferenced query |
| Any filter query (count, plus the first 100 rows) | 0.11–0.14 s (leaf_mass_per_area) | DuckDB reads the Parquet file directly, with `IN`: 0.03 s | Not noticeable per click. The value of O1 is correctness (B1, B2). |
| App Information tab ([R/mod_app_info.R:198-241](R/mod_app_info.R#L198-L241)) | Downloads all 6,149 telemetry rows in 7 requests for the headline counts, again for the monthly table, and again every 60 s while the tab is open. This grows with every session. | one request to a Supabase view that groups by month and event type | About 1–2 s less when opening the tab, and the cost stops growing |

The R process is also blocked outright:

- `Sys.sleep(2)` runs on every visit through a link with URL parameters ([R/svr_url_params.R:104](R/svr_url_params.R#L104)). The visitor waits 2 s, and so does everyone else.
- `Sys.sleep(0.1)` runs at [R/app_server.R:71](R/app_server.R#L71) and [R/srv_filter_updates.R:128](R/srv_filter_updates.R#L128).
- Telemetry is sent as a synchronous HTTP POST at every session start and every search ([R/telemetry_supabase.R](R/telemetry_supabase.R)). Each Supabase round trip took about 0.15 s from the test laptop. These calls could be made asynchronous, for example with `httr2::req_perform_promise()`, or batched.

One change is **not worth making**: copying the display table into an in-memory DuckDB table. It costs +2.7 GB of RAM to save about 10 ms over a view that reads the Parquet file directly.

**Not measured:** time spent in the browser. Rendering up to 10,000 rows in DT client-side, plotly charts with one point per record, and leaflet maps with one marker per record ([R/generate_trait_profile.R:113-122](R/generate_trait_profile.R#L113-L122)) may dominate some interactions, and the fixes above do not touch them. Also unmeasured: the server's size and how many users it has at once. Running `profvis` on the server, plus a short `shinyloadtest` run, would show where the end-to-end time goes.

## Code structure

- **One block copied four times.** The same "apply filters → count → collect 100 rows or all → update caches" block, about 50 lines, appears at [R/app_server.R:79](R/app_server.R#L79), [R/app_server.R:133](R/app_server.R#L133), [R/app_server.R:205](R/app_server.R#L205) and [R/svr_url_params.R:143](R/svr_url_params.R#L143). The copies have already drifted: the URL path hard-codes `dataset_type = "species"`. One `run_filter_query(parsed_filters)` helper would replace all four.
- **Code that never runs.**
  - DT never sends a sort input (it sends no `input$<id>_order` at all). With `filter = "none"` there is also no column-filter row, so `input$<id>_search_columns` never changes. The sort and column-filter observers ([R/mod_data_table.R:172-296](R/mod_data_table.R#L172-L296), about 120 lines) therefore never run. As a result, sorting reorders only the rows already loaded (100 rows when there are more than 10,000), although the help text says "Columns are sortable". Sorting in DuckDB would make that claim true.
  - Also unused: the visible-rows branch of the download handler ([R/srv_download.R:33-53](R/srv_download.R#L33-L53)); the `basis_of_record` and `life_stage` filters, which have no UI inputs; and `export_bibtex_for_data`, which is defined twice ([R/generate_taxon_text.R:301](R/generate_taxon_text.R#L301) and [R/generate_usage_text.R:79](R/generate_usage_text.R#L79)).
- **A package in name only.** [R/data_loading.R](R/data_loading.R) loads the data at the top level, calls `source("R/telemetry_supabase.R")` and `library(httr2)`, and sets global options; [R/mod_app_info.R:10](R/mod_app_info.R#L10) calls `library(DT)`. `apply_filters()` reads `trait_groups` and `controlled_vocab_columns` with `get(..., envir = .GlobalEnv)`. All of this works under `pkgload::load_all()` but not in an installed package. Moving the data loading into a function called from `app_server`, or from a `global.R`-style startup, would make the tests meaningful.
- **The data build can't be reproduced from the repo.** `prepare_data_for_portal()` reads `inst/extdata/austraits/trait_groups_for_portal.csv` ([R/fct_prepare_data.R:43-47](R/fct_prepare_data.R#L43-L47)), which is not committed. It also writes the species-averages Parquet twice as byte-identical files (49 MB each in the full build; [R/fct_prepare_data.R:75-78](R/fct_prepare_data.R#L75-L78)), and `data_loading.R` opens that one file three times.
- **Trait View map.** The text says "the size of the point indicating the number of records at that location", but every marker has `radius = 4` and there is one marker per record. Aggregating by location would make the text true and cut the marker count. The coordinate conversion runs a per-row `sapply()` with a `tryCatch()` ([R/generate_trait_profile.R:33-39](R/generate_trait_profile.R#L33-L39)); `suppressWarnings(as.numeric(x))` does the same thing vectorised.

## Tests

The suite has 94 tests; run locally against the full build, all pass and 22 are skipped. Some skips are conditional and some are unconditional `skip()` calls. None of them would have caught B1–B8, and one test asserts the B1 workaround. Rather than chasing coverage, add one regression test per bug, written so it fails on today's code:

- **B1:** for a bracketed taxon name, `apply_filters()` returns the same count as `taxon_name == x`.
- **B2:** a custom filter with two values returns rows, and `dataset_id = "Bryant_2021"` does not return `Bryant_2021_2`.
- **B4 and B7:** use `testServer()` to clear the filters, pick one taxon, switch to Taxon View, and assert that the session survives. Mock `log_telemetry_event()` and assert that a "Load next 100" click logs no `search`.
- **B5:** `generate_taxon_text()` succeeds for one real taxon, together with a data-contract test that checks each Parquet file for the columns the code reads. The contract test should also run on the server at deploy time, since that is where the data actually lives.
- **B6:** the rendered table widget has `escape` on.
- **B3:** this needs a real browser. A single `shinytest2` flow (apply with the top button, change a filter, apply with the bottom button, then download) would cover B3 and act as a smoke test for the whole app.

All of these can run on CI against the committed lite build. Delete the test at `test-filter-parsing.R:158` as part of O1.

**Load testing (`shinyloadtest`).** Record one realistic session with `shinyloadtest::record_session()` (open the app, apply a filter, open Trait View, download), replay it with `shinycannon` (a Java command-line tool) at 1, 5 and 10 concurrent users, and compare the reports with `shinyloadtest_report()`. This measures what a user actually waits for when others are using the app at the same time, which is the "one download freezes everyone" effect. Run it before and after O2 to get a before/after comparison. Don't point it at production: it would add load for real users and inflate the telemetry (B7). Use a local copy, or a second instance on the server with telemetry disabled.

**Load test results (10 October 2026).** The `shinyloadtest` recorder stalls on this app after relaying the first output message: the page stops responding to the browser driver. It records a minimal Shiny app fine. As a substitute, [dev/loadtest.R](dev/loadtest.R) runs N real headless browsers at once, each scripting the same visit, and times each step from action to idle. Both versions were run locally with `shiny::runApp()` on the full 7.0.0 data, with users arriving 2 s apart. Download times (median / slowest):

| Users | `master` | `fix/review-2026-10` |
|---|---|---|
| 1 | 13.6 s | 1.4 s |
| 5 | 32.8 / 49.2 s | 1.2 / 1.6 s |
| 10 | 40.5 / 57.8 s | 2.0 / 2.5 s |

On `master`, downloads queue behind each other in the single R process, so the tenth user waits almost a minute; on the branch they stay around 2 s. Opening the app, applying a filter and opening Trait View took 0.1–3 s on both versions at every load. Table paging now costs a server round trip (median 0.6 s at 5 users, 1.1 s at 10). On `master`, page 2 of a large result was not a server request at all, because only the first 100 rows were loaded. Caveats: the server and all browsers ran on one laptop (18 cores, 128 GB), and each user downloads at the end of the visit, so this measures downloads queueing behind downloads more than other steps queueing behind them.

## Deploy and CI

- **Deploys don't wait for CI.** [.github/workflows/auto-deploy.yml](.github/workflows/auto-deploy.yml) runs on every push to `master`, alongside R-CMD-check rather than after it. It then runs `systemctl restart shiny-server`, which drops every live session on the server, including APCalign-app's if that app is still hosted there. Making the deploy depend on a passing check (`workflow_run`, or `needs:` in a single workflow) would fix the first problem. Restarting only this app, for example by touching `restart.txt` in the app directory, would fix the second.
- **CI doesn't test the production platform.** CI runs on macOS and Windows; production runs Linux.
- **AGENTS.md is out of date.** It says the default branch is `develop`, which has been merged and deleted; the default is `master`.
- **Leftovers from the old deployment.** `manifest.json` (388 KB) and `rsconnect/` belong to the deprecated shinyapps.io deployment.

## Upstream (not a portal bug)

75 `measurement_remarks` values in the AusTraits 7.0.0 release contain the literal text `<U+00B0>` where a degree sign belongs. These are in the release `.rds` itself, so the fix belongs in austraits.build.

## Proposed order of work

| # | Change | Fixes | Status |
|---|---|---|---|
| O1 | Replace `duckdb_register_arrow()` with `CREATE VIEW … AS SELECT * FROM read_parquet(…)` so DuckDB reads the files directly. Use `%in%` for exact matches and rewrite the test that asserts `grepl`. Match `dataset_id` on whole list elements. | B1, B2 | done on `fix/review-2026-10`. `load_portal_data()` now creates `read_parquet()` views when the package loads (`.onLoad()`), so a fresh connection is made in the process that serves the app. |
| O2 | Build the download as one DuckDB query: `current_austraits() \|> dplyr::semi_join(filtered_query \|> dplyr::select(row_id), by = "row_id")`. Read `sources.bib` once at startup. | 10 s and 1.2 GB per download | done. The CSV is written by DuckDB `COPY` straight to disk, and the citations now cover every source in the download, not a 1,000-row sample. |
| O3 | Write one `run_filter_query()` helper to replace the four copies. Give the bottom Apply button its own ID and observe both buttons. Remove the `Sys.sleep()` calls. Delete the Taxon View fallback. Set `escape = TRUE`. | B3, B4, B6, blocking | done. Failed filters now show the user an error notification instead of silently emptying the table. |
| O4 | Add a startup check of the expected columns, and the regression tests listed under [Tests](#tests). Version the taxon-text cache by data release. Run a `shinyloadtest` baseline before O2. | B5, B8, tests | done. The load test used [dev/loadtest.R](dev/loadtest.R) instead of `shinyloadtest`, whose recorder stalls on this app; see [Tests](#tests) for the results. The required columns stop loading; a missing optional `value_range` prints a note, and Taxon View falls back to the most common value. Both checked-in data builds lack `value_range`, so rebuild them. The cache key is the data release plus a fingerprint of the species file. |
| O5 | Log `search` only on Apply and URL loads. Aggregate telemetry in a Supabase view. Send telemetry asynchronously. | B7, App Information tab | partly done. Search logging and async telemetry are done, and telemetry is off under tests (running the old suite locally wrote to production). The Supabase view is open; meanwhile telemetry reads are cached for 60 s across all sessions. |
| O6 | Do paging and sorting in DuckDB (`LIMIT`/`OFFSET`, or DT server-side processing backed by DuckDB). Require exactly one trait before Trait View collects anything. Store lat/lon as numbers. | load-more, Trait View, georeferenced filter, sorting | done. DataTables server-side processing replaces "Load next 100"; sorting covers the whole result, with number-like text columns sorted numerically. Lat/lon are cast to `DOUBLE` in the DuckDB view rather than at data prep. |
| O7 | Make deploys wait for CI, restart only this app, add a Linux CI job, and update AGENTS.md. | deploy risk | done in the workflow files; not yet exercised. Check on the server that the deploy user can `git merge --ff-only` and that touching `restart.txt` restarts the app. |

O1 should come first: it removes the reason for the regex workaround, and O2 and O6 build on the same direct-read views. Take the `shinyloadtest` baseline from O4 before starting O2; otherwise there is nothing to compare against.

## Reproducing the measurements

Run these from the repo root. `pkgload::load_all()` loads `austraits-7.0.0-full/` if it exists and the lite build otherwise.

```r
pkgload::load_all(".")

# B1: a bracketed taxon name returns 0 rows through apply_filters()
x  <- "(Dockrillia pugioniformis x Dockrillia striolata) x Dockrillia pugioniformis"
pf <- list(taxon = list(taxon_type = "taxon_name", taxon_name = x),
           trait = list(), location = list(), other = list(), custom = list())
apply_filters(austraits_display_duckdb, pf) |> dplyr::count() |> dplyr::collect()          # 0
austraits_display_duckdb |> dplyr::filter(taxon_name == x) |> dplyr::count() |> dplyr::collect()  # 3

# B2: IN with two values fails on the Arrow-registered table ...
austraits_display_duckdb |> dplyr::filter(life_stage %in% c("adult", "seedling")) |>
  dplyr::count() |> dplyr::collect()   # Error: Arrow table filter pushdown ... Not implemented

# ... and works when DuckDB reads the Parquet file directly (O1)
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, sprintf("CREATE VIEW display AS SELECT * FROM read_parquet('%s')",
                            file.path(data_path, "austraits-display.parquet")))
dplyr::tbl(con, "display") |> dplyr::filter(life_stage %in% c("adult", "seedling")) |>
  dplyr::count() |> dplyr::collect()

# B4: the Taxon View fallback with an unparsed filter list
apply_filters(austraits_display, list(taxon_type = "taxon_name", taxon_name = "Eucalyptus globulus",
                                      trait_filter_type = "name", location = ""))
# Error: $ operator is invalid for atomic vectors

# B5
generate_taxon_text("Eucalyptus globulus")   # Error: object 'value_range' not found

# Download path, current vs O2 (SQL IN subquery ~0.3 s; dbplyr semi_join() ~0.7 s)
system.time({
  ids <- austraits_duckdb |> dplyr::filter(trait_name == "leaf_mass_per_area") |>
    dplyr::select(row_id) |> dplyr::collect() |> dplyr::pull(row_id)
  austraits_duckdb |> dplyr::collect() |> dplyr::filter(row_id %in% ids)
})   # ~10 s
DBI::dbExecute(con, sprintf("CREATE VIEW data AS SELECT * FROM read_parquet('%s')",
                            file.path(data_path, "austraits-data.parquet")))
system.time(DBI::dbGetQuery(con,
  "SELECT * FROM data WHERE row_id IN (SELECT row_id FROM display WHERE trait_name IN ('leaf_mass_per_area'))"))
# ~0.3 s
```

The B6 count (`<<…>>` text in context-property columns) and the B2 `dataset_id` count come from simple DuckDB queries over `austraits-display.parquet`. The telemetry row count came from one read-only `HEAD`-style request using `Prefer: count=exact`.
