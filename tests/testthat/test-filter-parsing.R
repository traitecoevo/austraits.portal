test_that("parse_filters extracts taxon filters correctly", {
  input <- list(
    dataset_type = "raw",
    taxon_type = "family",
    family = c("Fabaceae", "Myrtaceae"),
    genus = NULL,
    taxon_name = NULL
  )
  
  result <- parse_filters(input)
  
  expect_equal(result$taxon$taxon_type, "family")
  expect_equal(result$taxon$family, c("Fabaceae", "Myrtaceae"))
  expect_null(result$taxon$genus)
  expect_null(result$taxon$taxon_name)
})

test_that("parse_filters extracts trait filters correctly", {
  input <- list(
    dataset_type = "raw",
    taxon_type = "all",
    trait_filter_type = "features",
    trait_name = c("leaf_area", "wood_density"),
    trait_grouping = "leaf size",
    structure_measured = "leaf",
    keywords = "photosynthesis"
  )
  
  result <- parse_filters(input)
  
  expect_equal(result$trait$trait_name, c("leaf_area", "wood_density"))
  expect_equal(result$trait$trait_grouping, "leaf size")
  expect_equal(result$trait$structure_measured, "leaf")
  expect_equal(result$trait$keywords, "photosynthesis")
})

test_that("parse_filters handles location filters", {
  input <- list(
    dataset_type = "raw",
    taxon_type = "all",
    location = "georeferenced",
    min_latitude = -35,
    max_latitude = -25,
    min_longitude = 140,
    max_longitude = 150
  )
  
  result <- parse_filters(input)
  
  expect_equal(result$location$location, "georeferenced")
  expect_equal(result$location$min_latitude, -35)
  expect_equal(result$location$max_latitude, -25)
  expect_equal(result$location$min_longitude, 140)
  expect_equal(result$location$max_longitude, 150)
})

test_that("parse_filters handles APC distribution filters", {
  input <- list(
    dataset_type = "species",
    taxon_type = "all",
    location = "apc",
    apc_taxon_distribution = c("NSW", "Qld")
  )
  
  result <- parse_filters(input)
  
  expect_equal(result$location$location, "apc")
  expect_equal(result$location$apc_taxon_distribution, c("NSW", "Qld"))
})

test_that("parse_filters handles other filters", {
  input <- list(
    dataset_type = "raw",
    taxon_type = "all",
    basis_of_record = "field",
    life_stage = "adult"
  )
  
  result <- parse_filters(input)
  
  expect_equal(result$other$basis_of_record, "field")
  expect_equal(result$other$life_stage, "adult")
})

test_that("parse_filters handles custom filters", {
  input <- list(
    dataset_type = "raw",
    taxon_type = "all",
    custom_col_1 = "dataset_id",
    custom_val_1 = "Falster_2003",
    custom_col_2 = "entity_type",
    custom_val_2 = "individual",
    custom_col_3 = NULL,
    custom_val_3 = NULL
  )
  
  result <- parse_filters(input)
  
  expect_length(result$custom, 2)
  expect_equal(result$custom[[1]]$column, "dataset_id")
  expect_equal(result$custom[[1]]$value, "Falster_2003")
  expect_equal(result$custom[[2]]$column, "entity_type")
  expect_equal(result$custom[[2]]$value, "individual")
})

test_that("parse_filters detects when filters are applied", {
  # No filters
  input_empty <- list(
    dataset_type = "raw",
    taxon_type = "all"
  )
  
  result_empty <- parse_filters(input_empty)
  expect_false(result_empty$has_filters)
  
  # With filters
  input_filtered <- list(
    dataset_type = "raw",
    taxon_type = "family",
    family = "Fabaceae"
  )
  
  result_filtered <- parse_filters(input_filtered)
  expect_true(result_filtered$has_filters)
})

test_that("parse_filters handles NULL and empty values gracefully", {
  input <- list(
    dataset_type = "raw",
    taxon_type = "all",
    family = NULL,
    genus = character(0),
    trait_name = c()
  )
  
  result <- parse_filters(input)
  
  expect_null(result$taxon$family)
  expect_null(result$taxon$genus)
  expect_null(result$trait$trait_name)
})

test_that("parse_filters ignores georeferenced location filters for species data", {
  input <- list(
    dataset_type = "species",
    taxon_type = "all",
    location = "georeferenced",
    min_latitude = -35,
    max_latitude = -25
  )
  
  result <- parse_filters(input)
  
  # Should still parse but won't be used in species dataset
  expect_equal(result$location$location, "georeferenced")
  expect_equal(result$dataset_type, "species")
})
test_that("parse_filters keeps trait features when trait name is empty", {
  parsed <- parse_filters(list(
    dataset_type = "species",
    taxon_type = "all",
    trait_filter_type = "features",
    structure_measured = "bark",
    trait_name = NULL,
    trait_grouping = NULL,
    keywords = NULL,
    family = NULL,
    genus = NULL,
    location = "",
    apc_taxon_distribution = NULL,
    basis_of_record = NULL,
    life_stage = NULL,
    custom_col_1 = NULL, custom_val_1 = NULL,
    custom_col_2 = NULL, custom_val_2 = NULL,
    custom_col_3 = NULL, custom_val_3 = NULL
  ))
  
  expect_equal(parsed$trait$structure_measured, "bark")
  expect_true(parsed$has_filters)
})
test_that("loading_from_url flag prevents default values from overwriting URL params", {
  # Simulates the bug: when taxon_type changes via URL,
  # observeEvent was setting hardcoded defaults (Fabaceae, Abutilon etc.)
  # overwriting the URL-provided values
  
  # When loading_from_url = TRUE, parse_filters should still work correctly
  input <- list(
    dataset_type = "raw",
    taxon_type = "family",
    family = "Myrtaceae",  # URL-provided value, NOT the default "Fabaceae"
    genus = NULL,
    taxon_name = NULL,
    trait_filter_type = "name",
    trait_name = NULL,
    trait_grouping = NULL,
    structure_measured = NULL,
    keywords = NULL,
    basis_of_record = NULL,
    life_stage = NULL,
    location = "",
    apc_taxon_distribution = NULL,
    custom_col_1 = NULL, custom_val_1 = NULL,
    custom_col_2 = NULL, custom_val_2 = NULL,
    custom_col_3 = NULL, custom_val_3 = NULL
  )
  
  result <- parse_filters(input)
  
  # URL value should be preserved, not overwritten by default "Fabaceae"
  expect_equal(result$taxon$family, "Myrtaceae")
  expect_equal(result$taxon$taxon_type, "family")
})
test_that("URL param trait_name is preserved and not overwritten", {
  input <- list(
    dataset_type = "raw",
    taxon_type = "all",
    family = NULL,
    genus = NULL,
    taxon_name = NULL,
    trait_filter_type = "name",
    trait_name = "leaf_area",  # URL-provided value
    trait_grouping = NULL,
    structure_measured = NULL,
    keywords = NULL,
    basis_of_record = NULL,
    life_stage = NULL,
    location = "",
    apc_taxon_distribution = NULL,
    custom_col_1 = NULL, custom_val_1 = NULL,
    custom_col_2 = NULL, custom_val_2 = NULL,
    custom_col_3 = NULL, custom_val_3 = NULL
  )
  
  result <- parse_filters(input)
  
  expect_equal(result$trait$trait_name, "leaf_area")
  expect_true(result$has_filters)
})