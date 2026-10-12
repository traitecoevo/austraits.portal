# austraits.portal (development version)

## Performance (October 2026, issue #130)

* The data table pages and sorts in DuckDB, so any result can be browsed and sorted in full; "Load next 100" is gone. Number-like text columns sort numerically.
* Downloads are built in DuckDB and the bibliography is read once per process: a 33k-row download went from 13–14 s to about 3 s, and with 10 users at once from 40–58 s to 2–2.5 s. Download citations now cover every source rather than a sample.
* Trait View collects data only when exactly one trait is selected, and only once.
* Telemetry is sent asynchronously, and App Information reads it once a minute for all sessions.
* No more `Sys.sleep()` calls blocking every user (2 s on each link with filters).
* `dev/loadtest.R` runs a concurrent-user load test with headless browsers.

## Correctness fixes (October 2026, issue #130)

* Taxon filters now match names containing brackets or parentheses; 3,906 of 33,370 taxa (12%) previously returned no rows. DuckDB reads the Parquet files directly instead of through Arrow, so filters use `%in%` rather than regular expressions.
* Multi-value custom filters work; `dataset_id` filters match whole IDs; free-text filters accept any characters.
* Both "Apply Filters" buttons work (they shared one id, so clicks were dropped).
* Taxon View no longer ends the session after "Clear Filters", and falls back gracefully on data builds without `value_range`; its cache is keyed by data release.
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