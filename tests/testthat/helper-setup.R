# Test helpers and setup

# Mock data generators for testing

create_mock_filter_input <- function(type = "basic") {
  if (type == "basic") {
    list(
      dataset_type = "raw",
      taxon_type = "all",
      trait_name = NULL,
      location = "",
      basis_of_record = NULL,
      life_stage = NULL
    )
  } else if (type == "filtered") {
    list(
      dataset_type = "raw",
      taxon_type = "family",
      family = "Fabaceae",
      trait_name = "leaf_area",
      location = "",
      basis_of_record = "field",
      life_stage = NULL
    )
  } else if (type == "complex") {
    list(
      dataset_type = "species",
      taxon_type = "genus",
      genus = c("Acacia", "Eucalyptus"),
      trait_name = c("leaf_area", "wood_density"),
      trait_grouping = "leaf size",
      location = "apc",
      apc_taxon_distribution = c("NSW", "Qld"),
      basis_of_record = c("field", "glasshouse"),
      life_stage = "adult",
      custom_col_1 = "dataset_id",
      custom_val_1 = "test_dataset"
    )
  }
}

# Skip conditions for tests requiring data

skip_if_no_data <- function() {
  if (is.null(get0("austraits_display_duckdb"))) {
    skip("Test requires austraits_display data to be loaded")
  }
}

skip_if_no_definitions <- function() {
  if (!exists("trait_definitions")) {
    skip("Test requires trait_definitions to be loaded")
  }
}

skip_if_no_trait_groups <- function() {
  if (!exists("trait_groups")) {
    skip("Test requires trait_groups to be loaded")
  }
}

# Mock DuckDB connection for testing
create_mock_duckdb_data <- function(data) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    skip("arrow package required for this test")
  }
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    skip("duckdb package required for this test")
  }
  
  # Convert data frame to arrow table
  arrow_table <- arrow::as_arrow_table(data)
  return(arrow_table)
}

# Test that a plot is valid
expect_valid_plot <- function(plot) {
  expect_s3_class(plot, "gg")
  expect_true("data" %in% names(plot) || "patch" %in% class(plot))
}

# Check HTML output is valid
expect_valid_html <- function(html) {
  expect_type(html, "character")
  expect_true(grepl("<", html))  # Contains HTML tags
  expect_true(nchar(html) > 0)
}

# Verify reactive values work
expect_reactive <- function(x) {
  expect_type(x, "closure")
  expect_s3_class(x, "reactive")
}
