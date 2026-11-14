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
    traits.use::austraits_lite |>
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

test_that("join_primary_source() handles both methods-table formats", {
  old <- traits.use::austraits_lite

  # Built before traits.build 2.3: key and citation come from methods as they
  # stand, and the key is not always the dataset_id
  joined_old <- join_primary_source(old)$traits
  expect_equal(nrow(joined_old), nrow(old$traits))
  expect_false(anyNA(joined_old$source_primary_key))
  expect_false(anyNA(joined_old$source_primary_citation))
  expect_true(any(joined_old$source_primary_key != joined_old$dataset_id))

  # Built with traits.build 2.3 or later: methods has dataset_id_citation and
  # no source_primary_key, which is always the dataset_id there
  new <- old
  new$methods <- new$methods |>
    dplyr::rename(dataset_id_citation = "source_primary_citation") |>
    dplyr::select(-"source_primary_key")
  joined_new <- join_primary_source(new)$traits
  expect_equal(nrow(joined_new), nrow(old$traits))
  expect_equal(joined_new$source_primary_key, joined_new$dataset_id)
  expect_equal(joined_new$source_primary_citation, joined_old$source_primary_citation)
  expect_false("dataset_id_citation" %in% names(joined_new))

  # Neither format: say so, rather than fail on a missing column
  neither <- old
  neither$methods <- neither$methods |> dplyr::select(-"source_primary_key", -"source_primary_citation")
  expect_error(join_primary_source(neither), "neither")
})

test_that("the display table has no reference columns besides the primary citation", {
  shown <- traits.use::austraits_lite |>
    join_primary_source() |>
    traits.use::flatten_database() |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    format_database_for_display()
  expect_false(any(c("dataset_id_type", "dataset_id_citation", "secondary_citation", "source_id_citation") %in% names(shown)))
  expect_true(all(c("source_primary_key", "source_primary_citation") %in% names(shown)))
  # traits.build's two reference ids: the dataset, and the source each observation came from
  expect_equal(names(shown)[match("dataset_id", names(shown)) + 1], "source_id")
  raw_source_id <- traits.use::austraits_lite$traits$source_id[shown$row_id]
  expect_false(anyNA(shown$source_id))
  expect_equal(shown$source_id[!is.na(raw_source_id)], raw_source_id[!is.na(raw_source_id)])
  expect_equal(shown$source_id[is.na(raw_source_id)], shown$dataset_id[is.na(raw_source_id)])
})

test_that("display data prepared before source_id was kept still loads", {
  old_columns <- setdiff(data_schema$display$required, "source_id")
  expect_no_error(suppressMessages(check_data_schema(old_columns, data_schema$display, "display")))
  expect_true("source_id" %in% columns_display)
})
