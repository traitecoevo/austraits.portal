# Test Coverage Summary

This document summarizes the test suite for the AusTraits Portal.

## Test Files Created

### Regression Tests

**test-regressions.R** - one or more tests per bug in issue #130 (October 2026 review): bracketed taxon names (B1), multi-value and `dataset_id` custom filters (B2), distinct Apply button ids (B3), Taxon View after an empty result (B4), taxon profiles and the data-column check (B5), HTML escaping and DuckDB paging and sorting (B6), search telemetry (B7), the download query, and telemetry being off under tests.

### Core Functionality Tests

1. **test-filter-parsing.R** - Filter parsing logic
   - Taxon filter extraction
   - Trait filter extraction
   - Location filters (georeferenced and APC distribution)
   - Other filters (basis of record, life stage)
   - Custom filters
   - Filter detection and validation
   - Edge cases (NULL, empty values)

2. **test-utils.R** - Utility functions
   - `add_target_blank()` function for HTML link modification
   - Multiple link handling
   - Edge cases (empty strings, no links)

3. **test-species-means.R** - Species averaging calculations
   - Numerical trait averaging
   - Categorical trait summaries
   - Mixed location and flora data handling
   - Statistical calculations (mean, min, max, median)

### Module Tests

4. **test-modules.R** - Shiny module testing using `testServer()`
   - `mod_filters_server` - Filter controls
   - `mod_data_table_server` - Data table display
   - `mod_citations_server` - Citation generation
   - `mod_app_info_server` - App information display
   - `mod_taxon_view_server` - Taxon profiles
   - `mod_trait_view_server` - Trait profiles

### Visualization Tests

5. **test-plotting.R** - Plot generation functions
   - `plot_trait_distribution()` - Main plotting dispatcher
   - `plot_trait_distribution_beeswarm()` - Numerical trait plots
   - `plot_categorical_trait_distribution()` - Categorical trait plots
   - Empty data handling
   - Single value handling
   - Log scale for wide ranges
   - Special characters in trait names

### Text Generation Tests

6. **test-text-generation.R** - Text/HTML generation
   - `generate_taxon_portal_links()` - External portal links
   - `export_bibtex_for_data()` - BibTeX export
   - `generate_usage_and_citations_text()` - Citation text
   - `convert_list_to_df1()` - List to data frame conversion
   - State/territory-specific flora links

### Data Preparation Tests

7. **test-data-preparation.R** - Data preprocessing
   - `prepare_data_for_portal()` - Full data preparation pipeline
   - (Mostly integration tests marked as manual/skip)

### Telemetry Tests

8. **test-telemetry.R** - Analytics tracking
   - `init_supabase_telemetry()` - Initialization
   - `start_telemetry_session()` - Session tracking
   - `log_telemetry_event()` - Event logging
   - `read_telemetry_metrics()` - Metrics retrieval
   - Local vs. cloud mode handling
   - Graceful failure with missing credentials

### Integration Tests

9. **test-app-integration.R** - Full app behavior
   - App launches without errors
   - UI contains all expected tabs
   - Filter sidebar presence
   - Module namespacing
   - CSS and JavaScript loading
   - Theme application
   - Download functionality

10. **test-edge-cases.R** - Error handling and edge cases
    - Empty strings
    - Malformed input
    - Missing columns
    - Special characters and Unicode
    - Very large datasets
    - Non-numeric strings in numeric fields
    - Long lists
    - Zero variance data
    - NA values
    - SQL injection attempts
    - XSS attempts

### Data Validation Tests

11. **test-data-validation.R** - Data integrity checks
    - Data structure schema validation
    - Dropdown cache completeness
    - Trait definitions loading
    - Trait groups data validity
    - Sources data formatting
    - Column definitions
    - Controlled vocabulary columns
    - Flora links structure
    - DuckDB initialization
    - Metadata loading
    - Trait-definition consistency

### Enhanced Tests

12. **test-golem-recommended.R** (updated)
    - App UI and server structure
    - Required dependencies check
    - Module pairing verification
    - Config functions
    - Reactive value initialization

### Test Helpers

13. **helper-setup.R** - Test utilities
    - `create_mock_trait_data()` - Generate test data
    - `create_mock_categorical_data()` - Categorical test data
    - `create_mock_filter_input()` - Mock filter inputs
    - `skip_if_no_data()` - Conditional test skipping
    - `expect_valid_plot()` - Plot validation
    - `expect_valid_html()` - HTML validation

## Test Coverage by Category

### Unit Tests (70%)
- Filter parsing functions
- Utility functions
- Species mean calculations
- Text generation
- Plotting functions
- Telemetry functions

### Integration Tests (20%)
- App initialization
- Module interactions
- Full filtering pipeline
- Data loading

### System Tests (10%)
- App launches
- UI rendering
- Data validation
- Security (XSS, SQL injection)

## Running Tests

```r
# Run all tests
testthat::test_dir("tests/testthat")

# Run specific test file
testthat::test_file("tests/testthat/test-filter-parsing.R")

# Run with coverage
covr::package_coverage()
```

## Test Dependencies

Tests require:
- `testthat` (>= 3.0.0)
- `shiny`
- `arrow`
- `dplyr`
- `ggplot2`
- `DT`
- `RefManageR`

Optional:
- `shinytest2` (for advanced app testing)
- `covr` (for coverage reports)

## Notes

- Some tests are marked with `skip()` for manual execution (data preparation, performance tests)
- Integration tests may be skipped on CRAN
- Data-dependent tests check for data availability before running
- Mock data generators in `helper-setup.R` allow testing without full database
