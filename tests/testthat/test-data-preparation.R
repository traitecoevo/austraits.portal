# Tests for data preparation functions

test_that("prepare_data_for_portal creates required files", {
  # The last step (APC synonyms) needs APCalign and the internet, and this test is not about it.
  # It is replaced here so that this test runs the same way on every computer.
  testthat::local_mocked_bindings(prepare_synonyms_for_portal = function(...) invisible(NULL))

  # Runs the whole preparation step on the small lite data set. This catches a missing input file
  # (for example inst/extdata/austraits/trait_groups_for_portal.csv, which prepare_data_for_portal() reads).
  temp_dir <- tempfile("portal_prep_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_no_error({
    austraits:::austraits_5.0.0_lite |>
      prepare_data_for_portal(temp_dir, overwrite = TRUE)
  })

  # Expected output files
  files <- c(
    "austraits-data.parquet",
    "austraits-display.parquet",
    "austraits-species-averages-display.parquet",
    "austraits-species-averages.parquet",
    "definitions.rds",
    "dropdown_cache.rds",
    "flora_links.rds",
    "metadata.rds",
    "sources.bib",
    "sources.rds",
    "trait_groups.rds"
  )

  # Check that expected files are created
  for (file in files) {
    expect_true(file.exists(file.path(temp_dir, file)), info = paste("Missing expected file:", file))
  }

  # These lists come from trait_groups_for_portal.csv, so they are empty if the file is missing
  cache <- readRDS(file.path(temp_dir, "dropdown_cache.rds"))
  expect_gt(length(cache$all_trait_groupings), 0)
  expect_gt(length(cache$all_keywords), 0)
})
