# austraits.portal (development version)

## AusTraits 8.0.0 and its published species means (November 2026, #140)

* The portal serves AusTraits 8.0.0. Species averages are the observed means published with the release (`austraits-8.0.0-observed-means`), not means the portal computes: `estimate_species_trait_means()` is gone. The table and downloads carry the published columns (`value`, `value_mode`, `value_count`, `n_records`, `n_datasets`, `method`, `reliability`, ...), described in the release's observed-means dictionary.
* Species means cover all 628 traits with data, not only the core traits. Taxon View lists the other traits in a second section, and an infraspecific taxon shows its species' means (the published means roll subspecies, varieties and forms up to the species).
* The app names the AusTraits version it serves (a badge linking to the release DOI), and so does every download: the zip and CSV names (`austraits-8.0.0-data`, `austraits-8.0.0-species-means`) and the usage text.
* Each download includes `recreate.R`, which loads the same release by its DOI with the austraits package (pinned to a tag) and applies the same filters, printed from the code the portal runs.
* Small species means no longer show as 0: the table rounds them to 3 significant figures instead of 2 decimals.
* The trait groups follow 8.0.0 (6 renamed, 32 dropped, 110 added), via the new `dev/update_trait_groups.R`. Four core traits no longer exist in 8.0.0: `leaf_hairs_juvenile_leaves`, `leaf_axil_angle`, `stem_density` and `pollination_system`.
* The release version and DOI come from the database and Zenodo (`release.rds`), replacing the hand-copied `austraits.json`. traits.use 1.0.1 or later is required.
* The species table leaves out names that APCalign could not align to the APC (an empty `taxon_rank`, so no family or genus): 3,913 of the 767,333 published means rows in 8.0.0, for example `"Tiny pink herb"`. `recreate.R` drops the same rows. The table now has 763,420 rows.

## Trait plots from traits.use (November 2026, traits.use#21)

* Trait View draws its plot with `traits.use::plot_trait_distribution()`, and the portal's own copies of the plot functions are gone. traits.use (0.1.1 or later) is now a dependency, installed from GitHub.
* Traits with negative values, such as `leaf_delta13C`, now show every value: the old axis limits shrank inwards for negative numbers and hid 1,135 of 5,686 `leaf_delta13C` values (20%).
* Axis labels read "plant_height (m)" rather than "plant_height  ( m )", and families with equal proportions in categorical plots are ordered alphabetically rather than by row order.
* With traits.use#30, categorical plots size each point by the number of taxa with a record of the value, as the "species count" legend says; the portal's version used a fractional total that under-counted taxa whose records disagree. Numeric plots draw `raw` values, the commonest value type, as open circles labelled "Raw" rather than diamonds labelled "Unknown", and the legend lists only the value types present.
* Deploys install any package the new commit needs before switching to it, so adding a dependency no longer needs a manual install on the server. If the install fails, the deploy stops and the previous version keeps serving.

## Performance (October 2026, issue #130)

* The data table pages and sorts in DuckDB, so any result can be browsed and sorted in full; "Load next 100" is gone. Number-like text columns sort numerically.
* Downloads are built in DuckDB and the bibliography is read once per process: a 33k-row download went from 13–14 s to about 3 s, and with 10 users at once from 40–58 s to 2–2.5 s. Download citations now cover every source rather than a sample.
* Trait View collects data only when exactly one trait is selected, and only once.
* Telemetry is sent asynchronously, and App Information reads it once a minute for all sessions.
* No more `Sys.sleep()` calls blocking every user (2 s on each link with filters).
* `dev/loadtest.R` runs a concurrent-user load test with headless browsers.

## Correctness fixes (October 2026, issue #130)

* Taxon filters now match names containing brackets or parentheses; 3,906 of 33,370 taxa (12%) previously returned no rows. DuckDB reads the Parquet files directly instead of through Arrow, so filters use `%in%` rather than regular expressions.
* Hybrid formulas such as "(Dockrillia pugioniformis x Dockrillia striolata) x Dockrillia pugioniformis" now sit in their genus; the genus dropdown listed both "(Dockrillia" and "Dockrillia" (#113).
* Multi-value custom filters work; `dataset_id` filters match whole IDs; free-text filters accept any characters.
* Both "Apply Filters" buttons work (they shared one id, so clicks were dropped).
* Taxon View no longer ends the session after "Clear Filters", and falls back gracefully on data builds without `value_range`; its cache is keyed by data release.
* Taxon View lists each taxon's synonyms and misapplied names from the Australian Plant Census, precomputed with APCalign by `prepare_synonyms_for_portal()` (#112).
* Context descriptions written as `<<…>>` are no longer swallowed by the data table.
* "Searches" telemetry counts only Apply clicks and links with filters; telemetry is off under tests.
* The first switch between species averages and raw data now reloads the table.
* Loading stops with a clear message if the data lacks a column the code reads.

## Major Changes

* **DuckDB Migration**: Replaced Arrow with DuckDB for 10-100x faster query performance
  - Lazy evaluation with instant table display (first 100 rows in ~0.5s)
  - Background counting with DuckDB (0.1s vs 1.8s with Arrow)
  - Memory-efficient streaming for large datasets

* **Species Averages Dataset**: Added full support for species-level aggregated data
  - Separate dataset with mean, min, max, median values per species
  - Automatic column mapping (mean_value → value) for compatibility
  - Trait view adapted to handle missing location data
  - Custom filtering for species-specific columns

* **Filter Performance Optimization**: Complete refactoring of filter system
  - Single-pass filtering with combined expressions (2x faster)
  - Debouncing (300ms) to prevent duplicate filter execution
  - Expression-based filtering eliminates sequential apply operations
  - Parse-once, apply-once architecture

## Code Organization

* **Golem Structure Refactoring**: Modularized codebase following best practices
  - `R/srv_*.R`: Server-side logic modules (filter updates, downloads, URL params, data loading)

* **Filter Architecture**:
  - `parse_filters()`: Centralized filter validation and structuring
  - `apply_filters()`: Single-pass expression-based filtering

## Performance Improvements

* **Lazy Loading**: Display first 100 rows if total rows are more than 10,000 (immediately), load more on demand
* **Smart Caching**: Full dataset cached after first "load more" request
* **Optimized Downloads**: Memory-efficient streaming for 100k+ row datasets

## UI Improvements

* **Dataset Banners**: Visual indicators for raw vs species data
* **Download Warnings**: Alert for datasets >100k rows
* **Pagination Text**: Cleaner "rows" terminology
* **Error Handling**: Graceful degradation for missing data

## Testing & Quality

* Added testing framework with spelling checks and wordlist
* Improved error messages with detailed logging
* Consistent code style across modules
* Refactored code again

## Previous Changes (from earlier version)

* Refactored code into golem structure with modular design
* Improved performance: lazy loading displays 100 rows at a time
* Optimized download to reduce memory usage (arrow streaming)
* Added URL query parameter support for deep linking
* Initial CRAN submission