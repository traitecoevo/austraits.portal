# austraits.portal — agent & contributor guide

`austraits.portal` is an R package providing a **Shiny interface to the AusTraits Plant Traits
database** — a code-free web portal for exploring and downloading trait data, deployed at
<https://app.austraits.org/austraits/> (the previous `unsw.shinyapps.io/austraits-portal/`
deployment is deprecated).

## Repo-local guidance

- **App entry point:** `app.R` runs `pkgload::load_all(".")` then
  `shiny::shinyApp(ui = app_ui, server = app_server)`. The top-level UI/server live in
  `R/app_ui.R` and `R/app_server.R`.
- **Modular Shiny (golem-style):** functionality is split into `mod_*` modules (e.g.
  `mod_filters`, `mod_data_table`, `mod_taxon_view`, `mod_trait_view`, `mod_citations`,
  `mod_app_info`), `fct_*` business logic (`fct_filter_parsing`, `fct_filter_application`,
  `fct_prepare_data`), `srv_*` server helpers, and `utils_*` helpers — all under `R/`.
- **Data layer:** trait data is stored as Parquet and queried by DuckDB, which reads the files directly through `read_parquet()` views. `load_portal_data()` (in `R/data_loading.R`) creates these views and the other data objects when the package loads (`.onLoad()` in `R/zzz.R`), so the connection belongs to the process that serves the app. Don't register Arrow datasets with DuckDB instead: on that path any `%in%` with two or more values fails, and the regex workaround it forced broke on 12% of taxon names (see `REVIEW.md`, B1). Filters use `%in%` for exact matches; the data table pages and sorts in DuckDB (DataTables server-side processing), so results are never loaded into R whole. Bundled data lives in `inst/extdata/austraits/` — a small `austraits-5.0.0-lite` set ships in the repo; the `austraits-7.0.0-full` set is prepared locally (see README "Data Preparation"). `data_schema` lists the columns the code reads, and loading stops if a required one is missing.
- **Data prep:** `prepare_data_for_portal()` (in `R/fct_prepare_data.R`) flattens the AusTraits
  database, computes species averages, exports Parquet, and caches metadata/dropdowns.
- **Config:** `config.yml` (via the `config` package, gitignored and build-ignored: it holds the Supabase key); deployment manifest in `manifest.json`. Telemetry is off under testthat, Shiny test mode, or `AUSTRAITS_TELEMETRY=off`; set that variable when running the app locally so you don't write to the production usage table.
- **Tests:** `tests/testthat/` (testthat edition 3) + `spelling`; see `tests/TEST_COVERAGE.md`.

Dev follows the standard R-package workflow plus a Shiny launch step: `devtools::load_all()` / `pkgload::load_all()`, then `shiny::shinyApp(ui = app_ui, server = app_server)` (or just source `app.R`); `devtools::test()`; `devtools::check()`. The default branch is `master` (`develop` has been merged and deleted). A push to `master` deploys to AWS once R-CMD-check passes on it (`.github/workflows/auto-deploy.yml`). Open review findings and their order of work are in `REVIEW.md`.

> Heads-up: the app needs prepared data files to run — only the `lite` set is in the repo, so a
> fresh checkout shows the demo subset until you build the full set. The live portal is now served
> from AWS at <https://app.austraits.org/austraits/>; the legacy shinyapps.io deployment
> (`rsconnect::deployApp()`, requiring `austraits.portal` installed from GitHub first and
> dependencies tracked in `manifest.json`) is deprecated and retained for reference only.

---

## AusTraits family — cross-package context

`austraits.portal` is part of the **AusTraits family** (a subset of the
[`traitecoevo`](https://github.com/traitecoevo) org) — here, the AusTraits web portal (interactive
data exploration/access). Family-wide concerns are documented centrally in
**[austraits-meta](https://github.com/traitecoevo/austraits-meta)** — don't restate them here, read
them there:

- **Start with [`AGENTS.md`](https://github.com/traitecoevo/austraits-meta/blob/main/AGENTS.md)** —
  pipeline order, who owns what, dependency direction, source-of-truth rules, cross-boundary
  artifacts, gotchas.
- **[`dependencies.yml`](https://github.com/traitecoevo/austraits-meta/blob/main/dependencies.yml)** —
  machine-readable package graph + cross-boundary artifacts.
- **[`governance/`](https://github.com/traitecoevo/austraits-meta/tree/main/governance)** —
  label taxonomy, board #9 conventions, release playbooks, triage.

**Filing issues:** the whole family is tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9) (new issues auto-add to it). Follow
the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md):
pick one work-type label (`bug` / `task` / `epic`); Status and Priority are set on the board, not as
labels.

**Commit messages:** every family repo squash-merges, so the **PR title and body become the permanent
commit message**. Keep the subject ≤50 characters as typed and the body ≤10 lines; put the working
detail — what you tried, benchmarks, test counts, rejected alternatives — in the **first PR comment**
instead. Full convention:
[`commit-messages.md`](https://github.com/traitecoevo/austraits-meta/blob/master/governance/commit-messages.md).

> austraits-meta is hand-maintained prose — a map, not ground truth. Verify specifics against the
> actual repos.
