# Tests for data validation and integrity

test_that("data structure follows expected schema", {
  skip_if_no_data()
  
  # Check that loaded data has expected columns
  expect_true("taxon_name" %in% colnames(austraits_display_duckdb))
  expect_true("trait_name" %in% colnames(austraits_display_duckdb))
  expect_true("value" %in% colnames(austraits_display_duckdb))
})

test_that("dropdown cache contains all required fields", {
  skip_if_not(exists("dropdowns"), "Dropdowns not loaded")
  
  required_fields <- c(
    "all_family", 
    "all_genus", 
    "all_taxon_names",
    "all_traits",
    "all_bor",
    "all_age",
    "all_states_territories",
    "all_trait_groupings"
  )
  
  for (field in required_fields) {
    expect_true(field %in% names(dropdowns), 
                info = paste("Missing required dropdown:", field))
  }
})

test_that("trait definitions are properly loaded", {
  skip_if_no_definitions()
  
  expect_type(trait_definitions, "list")
  expect_true(length(trait_definitions) > 0)
  
  # Check a sample trait has required fields
  if (length(trait_definitions) > 0) {
    first_trait <- trait_definitions[[1]]
    expect_true("type" %in% names(first_trait))
    expect_true("description" %in% names(first_trait))
  }
})

test_that("trait groups data is valid", {
  skip_if_no_trait_groups()
  
  expect_s3_class(trait_groups, "data.frame")
  expect_true("trait" %in% names(trait_groups))
  expect_true("trait_group_for_portal" %in% names(trait_groups))
})

test_that("sources data is properly formatted", {
  skip_if_not(exists("sources"), "Sources not loaded")
  
  expect_s3_class(sources, "data.frame")
  expect_true("source_primary_key" %in% names(sources))
  expect_true("source_primary_citation" %in% names(sources))
})

test_that("columns_display contains valid column names", {
  skip_if_not(exists("columns_display"), "columns_display not defined")
  
  expect_type(columns_display, "character")
  expect_true(length(columns_display) > 0)
  
  # Check for key columns
  expect_true("taxon_name" %in% columns_display)
  expect_true("trait_name" %in% columns_display)
  expect_true("value" %in% columns_display)
})

test_that("species columns are appropriate for species dataset", {
  skip_if_not(exists("columns_display_species"), "columns_display_species not defined")
  
  expect_type(columns_display_species, "character")
  
  # Species dataset should have aggregated columns
  expect_true("value_mean" %in% columns_display_species)
  expect_true("replicates" %in% columns_display_species)
  
  # Should not have observation-level columns
  expect_false("latitude (deg)" %in% columns_display_species)
  expect_false("longitude (deg)" %in% columns_display_species)
})

test_that("controlled vocabulary columns are properly defined", {
  skip_if_not(exists("controlled_vocab_columns"), "controlled_vocab_columns not defined")
  
  expect_type(controlled_vocab_columns, "character")
  expect_true(length(controlled_vocab_columns) > 0)
  
  # Check for expected controlled vocab columns
  expect_true("dataset_id" %in% controlled_vocab_columns)
  expect_true("basis_of_record" %in% controlled_vocab_columns)
  expect_true("life_stage" %in% controlled_vocab_columns)
})

test_that("custom filter columns are properly defined", {
  skip_if_not(exists("custom_filter_columns"), "custom_filter_columns not defined")
  
  expect_type(custom_filter_columns, "character")
  expect_true(length(custom_filter_columns) > 0)
})

test_that("flora links are properly structured", {
  skip_if_not(exists("flora_links"), "flora_links not loaded")
  
  expect_type(flora_links, "list")
  
  # Should have links for different states
  expect_true("atrp" %in% names(flora_links) || 
              "nt" %in% names(flora_links) || 
              "vic" %in% names(flora_links))
})

test_that("DuckDB connection is properly initialized", {
  skip_if_not(exists("duckdb_con"), "DuckDB connection not initialized")
  
  expect_s4_class(duckdb_con, "DBIConnection")
})

test_that("DuckDB tables are registered", {
  skip_if_not(exists("austraits_display_duckdb"), "DuckDB tables not registered")
  
  expect_s3_class(austraits_display_duckdb, "tbl_duckdb_connection")
})

test_that("data path is correctly set", {
  skip("Data path validation depends on runtime environment setup")
  skip_if_not(exists("data_path"), "data_path not set")
  
  expect_type(data_path, "character")
  expect_true(dir.exists(data_path))
})

test_that("metadata is loaded", {
  skip_if_not(exists("metatdata"), "Metadata not loaded")
  
  expect_type(metatdata, "list")
  # Check for Zenodo-like structure
  expect_true("hits" %in% names(metatdata) || length(metatdata) > 0)
})

test_that("dropdowns contain actual data", {
  skip_if_not(exists("dropdowns"), "Dropdowns not loaded")
  
  # Each dropdown should have values
  expect_true(length(dropdowns$all_family) > 0)
  expect_true(length(dropdowns$all_genus) > 0)
  expect_true(length(dropdowns$all_traits) > 0)
})

test_that("trait groups match available traits", {
  skip_if_no_trait_groups()
  skip_if_no_definitions()
  
  # Trait groups should reference valid traits
  traits_in_groups <- unique(trait_groups$trait)
  traits_in_definitions <- names(trait_definitions)
  
  # Most traits in groups should be in definitions
  overlap <- mean(traits_in_groups %in% traits_in_definitions)
  expect_true(overlap > 0.8, 
              info = "Less than 80% of trait group traits found in definitions")
})
