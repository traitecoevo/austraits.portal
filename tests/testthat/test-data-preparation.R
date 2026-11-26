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
  expect_equal(nrow(species), sum(!is.na(means$taxon_rank)))
  expect_true(all(names(means) %in% names(species)))
  expect_equal(readRDS(file.path(temp_dir, "release.rds")), release)
})

test_that("prepare_species_means() keeps the published means and adds the portal's columns", {
  database <- traits.use::austraits_lite
  flattened <- database |> join_primary_source() |> traits.use::flatten_database()
  means <- traits.use::summarise_trait_means(database)
  kept <- means[!is.na(means$taxon_rank), ]
  out <- prepare_species_means(means, database, flattened)

  expect_equal(nrow(out), nrow(kept))
  expect_equal(out$value, kept$value)
  expect_equal(out$taxon_name, kept$taxon_name)
  expect_true(all(c("source_primary_key", "source_primary_citation", "taxon_distribution", "row_id") %in% names(out)))
  expect_equal(out$source_primary_key, kept$dataset_id)
  expect_equal(out$row_id, seq_len(nrow(kept)))
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

test_that("prepare_species_means() leaves out names APCalign could not align to the APC", {
  database <- traits.use::austraits_lite
  flattened <- database |> join_primary_source() |> traits.use::flatten_database()
  means <- traits.use::summarise_trait_means(database)
  aligned <- means[!is.na(means$taxon_rank), ]

  # Two unmatched names (no rank, no family or genus), first in the table
  unmatched <- aligned[1:2, ]
  unmatched$taxon_name <- c("\"Tiny pink herb\"", "Not an APC name")
  unmatched$taxon_rank <- NA_character_
  unmatched$family <- NA_character_
  unmatched$genus <- NA_character_
  mixed <- rbind(unmatched, means)

  expect_message(out <- prepare_species_means(mixed, database, flattened), "Left out")
  expect_false(anyNA(out$taxon_rank))
  expect_false(any(out$taxon_name %in% unmatched$taxon_name))
  expect_equal(nrow(out), nrow(aligned))
  expect_equal(out$taxon_name, aligned$taxon_name)
  # row_id counts the rows that are kept, from 1
  expect_equal(out$row_id, seq_len(nrow(aligned)))
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

# austraits_lite in either methods-table format, whichever form traits.use
# ships it in (AusTraits 7.0.0 before traits.use 1.0.0, data model 1.4.0 after)
with_methods_format <- function(database, format) {
  methods <- database$methods
  if (format == "old" && !"source_primary_key" %in% names(methods)) {
    # as AusTraits 7.0.0 had it: a dataset could be keyed by another reference
    methods <- methods |>
      dplyr::rename(source_primary_citation = "dataset_id_citation") |>
      dplyr::mutate(source_primary_key = dplyr::if_else(dataset_id == "Kew_2019_2", "Kew_2019", dataset_id),
                    .after = "dataset_id")
  }
  if (format == "new" && "source_primary_key" %in% names(methods)) {
    methods <- methods |>
      dplyr::rename(dataset_id_citation = "source_primary_citation") |>
      dplyr::select(-"source_primary_key")
  }
  database$methods <- methods
  database
}

test_that("join_primary_source() handles both methods-table formats", {
  old <- with_methods_format(traits.use::austraits_lite, "old")

  # Built before traits.build 2.3: key and citation come from methods as they
  # stand, and the key is not always the dataset_id
  joined_old <- join_primary_source(old)$traits
  expect_equal(nrow(joined_old), nrow(old$traits))
  expect_false(anyNA(joined_old$source_primary_key))
  expect_false(anyNA(joined_old$source_primary_citation))
  expect_true(any(joined_old$source_primary_key != joined_old$dataset_id))

  # Built with traits.build 2.3 or later: methods has dataset_id_citation and
  # no source_primary_key, which is always the dataset_id there
  new <- with_methods_format(traits.use::austraits_lite, "new")
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
  # Databases built before traits.build 2.3 leave source_id missing on most
  # rows; austraits_lite from traits.use 1.0.0 fills it on every row, so
  # blank some to check that those show the dataset_id
  database <- traits.use::austraits_lite
  database$traits$source_id[seq(1, nrow(database$traits), by = 5)] <- NA_character_
  shown <- database |>
    join_primary_source() |>
    traits.use::flatten_database() |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    format_database_for_display()
  expect_false(any(c("dataset_id_type", "dataset_id_citation", "secondary_citation", "source_id_citation") %in% names(shown)))
  expect_true(all(c("source_primary_key", "source_primary_citation") %in% names(shown)))
  # traits.build's two reference ids: the dataset, and the source each observation came from
  expect_equal(names(shown)[match("dataset_id", names(shown)) + 1], "source_id")
  raw_source_id <- database$traits$source_id[shown$row_id]
  expect_true(anyNA(raw_source_id))
  expect_false(anyNA(shown$source_id))
  expect_equal(shown$source_id[!is.na(raw_source_id)], raw_source_id[!is.na(raw_source_id)])
  expect_equal(shown$source_id[is.na(raw_source_id)], shown$dataset_id[is.na(raw_source_id)])
})

test_that("display data prepared before source_id was kept still loads", {
  old_columns <- setdiff(data_schema$display$required, "source_id")
  expect_no_error(suppressMessages(check_data_schema(old_columns, data_schema$display, "display")))
  expect_true("source_id" %in% columns_display)
})
