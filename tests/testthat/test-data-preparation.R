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

  # A fixed release record, so the test needs no internet
  release <- list(version = "7.0.0.9000", doi = NA_character_, concept_doi = "10.5281/zenodo.3568417",
                  publication_date = NA_character_, austraits_package = NA_character_)
  expect_no_error({
    traits.use::austraits_lite |>
      prepare_data_for_portal(temp_dir, release = release, overwrite = TRUE)
  })

  # Expected output files
  files <- c(
    "austraits-data.parquet",
    "austraits-display.parquet",
    "austraits-species-averages.parquet",
    "definitions.rds",
    "dropdown_cache.rds",
    "flora_links.rds",
    "release.rds",
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

  # The species table is the means summarise_trait_means() builds (the
  # function behind the published file), with the portal's columns added
  means <- traits.use::summarise_trait_means(traits.use::austraits_lite)
  species <- arrow::read_parquet(file.path(temp_dir, "austraits-species-averages.parquet"))
  expect_equal(nrow(species), nrow(means))
  expect_true(all(names(means) %in% names(species)))
  expect_equal(readRDS(file.path(temp_dir, "release.rds")), release)
})

test_that("prepare_species_means() keeps the published means and adds the portal's columns", {
  database <- traits.use::austraits_lite
  flattened <- database |> join_primary_source() |> traits.use::flatten_database()
  means <- traits.use::summarise_trait_means(database)
  out <- prepare_species_means(means, database, flattened)

  expect_equal(nrow(out), nrow(means))
  expect_equal(out$value, means$value)
  expect_equal(out$taxon_name, means$taxon_name)
  expect_true(all(c("source_primary_key", "source_primary_citation", "taxon_distribution", "row_id") %in% names(out)))
  expect_equal(out$source_primary_key, means$dataset_id)
  expect_equal(out$row_id, seq_len(nrow(means)))
  # Each row cites its datasets' own references, in the order of dataset_id
  expect_false(anyNA(out$source_primary_citation))
  single <- out[!grepl("; ", out$dataset_id), ][1, ]
  expect_equal(single$source_primary_citation,
               unique(flattened$source_primary_citation[flattened$source_primary_key == single$dataset_id]))
  pair <- out[lengths(strsplit(out$dataset_id, "; ")) == 2, ][1, ]
  ids <- strsplit(pair$dataset_id, "; ")[[1]]
  expect_equal(pair$source_primary_citation, paste(vapply(ids, function(id)
    unique(flattened$source_primary_citation[flattened$source_primary_key == id]), ""), collapse = "; "))
  # The portal reads every column it requires
  expect_true(all(data_schema$species$required %in% names(out)))
  # No dataset is cited as the text "NA"
  expect_false(any(grepl("(^|; )NA(;|$)", out$source_primary_citation)))
  expect_error(prepare_species_means(means[, c("taxon_name", "trait_name")], database, flattened), "lack columns")
})

test_that("species means cite each dataset by its own reference key in older builds", {
  # Before traits.build 2.3 the key could differ from the dataset_id:
  # Westoby_2014 cites Gleason_2012. The 5.0.0 lite set is such a build.
  database <- traits.use:::austraits_5.0.0_lite
  flattened <- database |> join_primary_source() |> traits.use::flatten_database()
  skip_if_not("Westoby_2014" %in% flattened$dataset_id, "Westoby_2014 not in this lite set")
  key <- unique(flattened$source_primary_key[flattened$dataset_id == "Westoby_2014"])
  means <- traits.use::summarise_trait_means(database)
  out <- prepare_species_means(means, database, flattened)
  only <- out[out$dataset_id == "Westoby_2014", ]
  skip_if(nrow(only) == 0, "No single-dataset Westoby_2014 means")
  expect_true(all(only$source_primary_key == key))
  expect_false(anyNA(only$source_primary_citation))
  expect_false(any(grepl("(^|; )NA(;|$)", out$source_primary_citation)))
})

test_that("portal_release_info() finds the release DOI from the version", {
  database <- traits.use::austraits_lite
  database$build_info$version <- "8.0.0"
  versions <- data.frame(version = c("8.0.0", "v7.0.0"),
                         doi = c("10.5281/zenodo.24543275", "10.5281/zenodo.15718081"),
                         publication_date = as.Date(c("2026-11-26", "2025-06-20")))
  release <- portal_release_info(database, versions)
  expect_equal(release$version, "8.0.0")
  expect_equal(release$doi, "10.5281/zenodo.24543275")
  expect_equal(release$publication_date, "2026-11-26")
  expect_equal(release$concept_doi, database$metadata$doi)

  # Versions on Zenodo carry a leading "v" up to 7.0.0
  database$build_info$version <- "7.0.0"
  expect_equal(portal_release_info(database, versions)$doi, "10.5281/zenodo.15718081")

  # A development build has no release DOI
  database$build_info$version <- "8.0.0.9000"
  expect_true(is.na(portal_release_info(database, versions)$doi))
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
