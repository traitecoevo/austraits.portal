# The R script in each download must select the same rows as the portal. These
# tests run the script's code against the database the loaded portal data was
# built from, and compare with the portal's own query.

filters_for <- function(dataset_type, ...) {
  filters <- list(taxon = list(), trait = list(), location = list(), other = list(),
                  custom = list(), dataset_type = dataset_type)
  args <- list(...)
  filters[names(args)] <- args
  filters
}

# The database behind the loaded portal data: the lite set's, or a release
# file next to the full set (as the README's data preparation leaves it)
recreate_inputs <- function() {
  if (is.null(get0("release")) || is.null(get0("data_path"))) return(NULL)
  if (grepl("lite", data_path)) {
    database <- traits.use:::austraits_5.0.0_lite
    return(list(austraits = database, means = traits.use::summarise_trait_means(database)))
  }
  dir <- dirname(data_path)
  files <- file.path(dir, sprintf("austraits-%s%s.rds", release$version, c("", "-observed-means")))
  if (!all(file.exists(files))) return(NULL)
  list(austraits = readRDS(files[1]), means = readRDS(files[2]))
}

run_script <- function(parsed, inputs) {
  view <- if (parsed$dataset_type == "species") austraits_species_duckdb else austraits_display_duckdb
  lines <- recreate_script(parsed, parsed$dataset_type, release, colnames(view), load = FALSE)
  env <- new.env()
  env$austraits <- inputs$austraits
  env$means <- inputs$means
  eval(parse(text = lines), envir = env)
  env$data
}

expect_same_rows <- function(parsed, inputs, name) {
  view <- if (parsed$dataset_type == "species") austraits_species_duckdb else austraits_display_duckdb
  utils::capture.output(portal <- apply_filters(view, parsed) |>
    dplyr::select(taxon_name, trait_name) |> dplyr::collect())
  script <- run_script(parsed, inputs)
  expect_gt(nrow(portal), 0)
  expect_equal(nrow(script), nrow(portal), info = name)
  expect_equal(sort(paste(script$taxon_name, script$trait_name)),
               sort(paste(portal$taxon_name, portal$trait_name)), info = name)
}

test_that("the download script recreates the portal's rows", {
  skip_if_no_data()
  skip_if_not_installed("austraits")
  inputs <- recreate_inputs()
  skip_if(is.null(inputs), "The database behind the loaded portal data is not available")

  families <- dropdowns$all_family
  family <- if ("Proteaceae" %in% families) "Proteaceae" else families[1]
  trait <- if ("leaf_mass_per_area" %in% dropdowns$all_traits) "leaf_mass_per_area" else dropdowns$all_traits[1]
  ids <- austraits_display_duckdb |> dplyr::filter(trait_name == !!trait) |>
    dplyr::distinct(dataset_id) |> dplyr::collect() |> dplyr::pull() |> sort() |> utils::head(2)

  cases <- list(
    raw_taxon_trait = filters_for("raw",
      taxon = list(taxon_type = "family", family = family),
      trait = list(trait_filter_type = "name", trait_name = trait)),
    raw_custom = filters_for("raw",
      trait = list(trait_filter_type = "name", trait_name = trait),
      custom = list(list(column = "dataset_id", value = ids),
                    list(column = "source_primary_citation", value = "a"))),
    raw_apc = filters_for("raw",
      trait = list(trait_filter_type = "name", trait_name = trait),
      location = list(location = "apc", apc_taxon_distribution = c("WA", "Tas"))),
    raw_box = filters_for("raw",
      trait = list(trait_filter_type = "name", trait_name = trait),
      location = list(location = "georeferenced", min_latitude = -40, max_latitude = -30,
                      min_longitude = 140, max_longitude = 155)),
    raw_features = filters_for("raw",
      taxon = list(taxon_type = "family", family = family),
      trait = list(trait_filter_type = "features", trait_grouping = "leaf morphology")),
    species_taxon_trait = filters_for("species",
      taxon = list(taxon_type = "family", family = family),
      trait = list(trait_filter_type = "name", trait_name = trait)),
    species_apc = filters_for("species",
      trait = list(trait_filter_type = "name", trait_name = trait),
      location = list(location = "apc", apc_taxon_distribution = "NSW")),
    species_custom = filters_for("species",
      taxon = list(taxon_type = "family", family = family),
      custom = list(list(column = "reliability", value = c("high", "medium")),
                    list(column = "method", value = "geometric"))),
    species_dataset = filters_for("species",
      trait = list(trait_filter_type = "name", trait_name = trait),
      custom = list(list(column = "dataset_id", value = ids)))
  )
  for (name in names(cases)) {
    expect_same_rows(cases[[name]], inputs, name)
  }
})

test_that("the download script pins the release and the package", {
  release <- list(version = "8.0.0", doi = "10.5281/zenodo.24543275", concept_doi = "10.5281/zenodo.3568417",
                  publication_date = "2026-11-26", austraits_package = "v4.0.1")
  parsed <- filters_for("species", trait = list(trait_filter_type = "name", trait_name = "plant_height"))
  script <- recreate_script(parsed, "species", release, c("taxon_name", "trait_name"), date = as.Date("2026-11-27"))
  expect_true(any(grepl('means <- austraits::load_austraits(doi = "10.5281/zenodo.24543275", means = TRUE)', script, fixed = TRUE)))
  expect_true(any(grepl('remotes::install_github("traitecoevo/austraits@v4.0.1")', script, fixed = TRUE)))
  expect_true(any(grepl("AusTraits 8.0.0, doi:10.5281/zenodo.24543275", script, fixed = TRUE)))
  expect_true(any(grepl("austraits-8.0.0-species-means.csv", script, fixed = TRUE)))
  expect_true(any(grepl('trait_name %in% "plant_height"', script, fixed = TRUE)))
  # Parses as R
  expect_no_error(parse(text = script))

  # A development build has no DOI: load by version
  release$doi <- NA_character_
  script <- recreate_script(parsed, "species", release, c("taxon_name", "trait_name"))
  expect_true(any(grepl('load_austraits(version = "8.0.0", means = TRUE)', script, fixed = TRUE)))
})

test_that("downloads are named after the release", {
  release <- list(version = "8.0.0")
  expect_equal(download_stem(release, "raw"), "austraits-8.0.0-data")
  expect_equal(download_stem(release, "species"), "austraits-8.0.0-species-means")
})

test_that("an infraspecific taxon shows its species' means", {
  expect_equal(species_means_name("Grevillea rosmarinifolia subsp. glabella"), "Grevillea rosmarinifolia")
  expect_equal(species_means_name("Eucalyptus saligna"), "Eucalyptus saligna")
  expect_equal(species_means_name("Acacia sp. Foo (A. B 123)"), "Acacia sp. Foo (A. B 123)")
  # A hybrid formula is not rolled up to its first parent
  expect_equal(species_means_name("Acacia adoxa var. adoxa x Acacia spondylophylla"),
               "Acacia adoxa var. adoxa x Acacia spondylophylla")
})

test_that("a custom filter for the other dataset is not applied", {
  # Switching dataset re-runs the filters before the inputs drop a custom
  # filter set for the other dataset
  stale <- filters_for("species", custom = list(list(column = "source_primary_citation", value = "Falster")))
  expect_length(filter_expressions(stale, c("taxon_name", "source_primary_citation")), 0)
  stale_raw <- filters_for("raw", custom = list(list(column = "reliability", value = "high")))
  expect_length(filter_expressions(stale_raw, c("taxon_name")), 0)
  kept <- filters_for("species", custom = list(list(column = "reliability", value = "high")))
  expect_length(filter_expressions(kept, c("taxon_name", "reliability")), 1)
})
