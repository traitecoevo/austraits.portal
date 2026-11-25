#' Prepare AusTraits data for the portal
#'
#' @description Flattens the AusTraits database, saves definitions and sources,
#'   and writes parquet files for data and display. The species table is the
#'   observed species means published with the release, with only the columns
#'   the portal needs to cite and filter it added.
#' @param austraits The AusTraits database object
#' @param output_dir Directory to write output files
#' @param means The release's published observed means, as returned by
#'   `austraits::load_austraits(version = , means = TRUE)`. If NULL, they are
#'   computed from `austraits` with `traits.use::summarise_trait_means()`, the
#'   function that builds the published file (used for the lite set).
#' @param release The release record from [portal_release_info()]; looked up
#'   on Zenodo from the database's version if NULL.
#' @param overwrite Logical; overwrite existing files if TRUE
#' @keywords internal
#' @noRd
prepare_data_for_portal <- function(austraits, output_dir, means = NULL, release = NULL,
                                    overwrite = FALSE) {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  ## Prepare the database and export (if doesn't already exist)
  filename_data <- file.path(output_dir, "austraits-data.parquet")
  filename_display <- file.path(output_dir, "austraits-display.parquet")
  
  ## Early exit if file already exists
  if (file.exists(filename_data) & !overwrite) { 
    message("✓ Data files already exist (use overwrite=TRUE to regenerate)")
    return()
  }

  # Which release this is, and its DOI, for the app text and downloads
  if (is.null(release)) release <- portal_release_info(austraits)
  message(sprintf("Preparing AusTraits %s (doi: %s)", release$version, release$doi))

  # Flatten the database
  message("Flattening AusTraits database...")
  austraits_full_flatten <-
    austraits |>
    join_primary_source() |>
    traits.use::flatten_database() |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    # Hybrid formulas starting "(Genus species x ...)" get the genus "(Genus"
    # upstream (APCalign); strip the bracket (issue #113)
    dplyr::mutate(genus = stringr::str_remove(genus, "^\\(")) |>
    dplyr::mutate(
      measurement_remarks = iconv(measurement_remarks,
        from = "", # Let's R guess the current encoding
        to = "UTF-8", # Convert to UTF-8
        sub = "" # Remove any bytes that can't be converted
      )
    )
  message("✓ Database flattened")

  # Species means: the observed means published with the release
  if (is.null(means)) {
    message("No published means given; computing them with traits.use::summarise_trait_means()...")
    means <- traits.use::summarise_trait_means(austraits)
  }
  austraits_species_averages <- prepare_species_means(means, austraits, austraits_full_flatten)
  message(sprintf("✓ Species means: %s rows, %s traits",
                  format(nrow(austraits_species_averages), big.mark = ","),
                  dplyr::n_distinct(austraits_species_averages$trait_name)))

  trait_groups <- readr::read_csv(
    system.file("extdata/austraits/trait_groups_for_portal.csv", package = "austraits.portal"),
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  # Save the flattened database
  message("Writing parquet files...")
  austraits_full_flatten |>
    arrow::write_parquet(filename_data)
  
  # Save the display version of the flattened database
  austraits_full_flatten |>
    format_database_for_display() |>
    arrow::write_parquet(filename_display)
  
  # Save the species means
  austraits_species_averages |>
    arrow::write_parquet(file.path(output_dir, "austraits-species-averages.parquet"))
  message("✓ Parquet files saved") 
  
  # Saving definitions
  message("Saving definitions...")
  austraits$definitions |> yaml::write_yaml(file.path(output_dir, "definitions.yml"))
  saveRDS(austraits$definitions, file.path(output_dir, "definitions.rds"))
  message("✓ Definitions saved")

  # Sources
  message("Saving sources...")
  austraits$sources |> RefManageR::WriteBib(file.path(output_dir, "sources.bib"))
  sources_df <- austraits_full_flatten |> 
    dplyr::select(source_primary_key, source_primary_citation) |>
    dplyr::distinct()
  saveRDS(sources_df, file.path(output_dir, "sources.rds"))
  message("✓ Sources saved")
  
  # Save trait_groups as RDS for faster loading
  message("Saving trait groups...")
  saveRDS(trait_groups, file.path(output_dir, "trait_groups.rds"))
  message("✓ Trait groups saved")
  
  # The release record: version and DOIs shown in the app and in downloads
  saveRDS(release, file.path(output_dir, "release.rds"))
  message("✓ Release record saved")
  
  # Combine and save state flora links as single RDS
  message("Consolidating flora links...")
  flora_links <- list(
    atrp = readr::read_csv(system.file("extdata/ATRP_links.csv", package = "austraits.portal"), show_col_types = FALSE) |>
      dplyr::rename(url = formatted) |> 
      dplyr::select(taxon_name, url) |>
      dplyr::filter(!is.na(url), url != ""),
    nt = readr::read_csv(system.file("extdata/NT_links.csv", package = "austraits.portal"), show_col_types = FALSE) |>
      dplyr::select(taxon_name, url) |> 
      dplyr::filter(!is.na(url), url != ""),
    vic = readr::read_csv(system.file("extdata/Vic_links.csv", package = "austraits.portal"), show_col_types = FALSE) |>
      dplyr::select(taxon_name, url) |> 
      dplyr::filter(!is.na(url), url != "")
  )
  saveRDS(flora_links, file.path(output_dir, "flora_links.rds"))
  message("✓ Flora links saved")
  
  # Cache dropdown values for faster app startup
  message("Caching dropdown values...")
  
  dropdown_cache <- list(
    all_family = austraits_full_flatten |> dplyr::distinct(family) |> dplyr::pull() |> sort(),
    all_genus = austraits_full_flatten |> dplyr::distinct(genus) |> dplyr::pull() |> sort(),
    all_taxon_names = austraits_full_flatten |> dplyr::distinct(taxon_name) |> dplyr::pull() |> sort(),
    all_traits = austraits_full_flatten |> dplyr::distinct(trait_name) |> dplyr::pull() |> sort(),
    all_bor = austraits_full_flatten |> dplyr::distinct(basis_of_record) |> dplyr::pull() |> sort(),
    all_age = austraits_full_flatten |> dplyr::distinct(life_stage) |> dplyr::pull() |> sort()
  )
  
  # Process states/territories
  dropdown_cache$all_states_territories <- austraits_full_flatten |> 
    dplyr::distinct(taxon_distribution) |> 
    dplyr::pull() |> 
    paste(collapse = ", ") |> 
    stringr::str_split(",") |> 
    purrr::map(~trimws(.x)) |> 
    purrr::list_c() |>
    unique() |> 
    stringr::word(1) |> 
    unique() |> 
    sort()
  
  # Add trait groupings and keywords
  dropdown_cache$all_trait_groupings <- trait_groups |>
    dplyr::pull(trait_group_for_portal) |>
    unique() |>
    sort()
  
  dropdown_cache$all_structure_measured <- trait_groups |>
    dplyr::pull(structure_measured) |>
    stringr::str_remove_all("\\[.*?\\]") |> 
    stringr::str_split("; |,") |>              
    unlist() |>                              
    stringr::str_trim() |>
    unique() |>
    sort()
  
  dropdown_cache$all_keywords <- trait_groups |>
    dplyr::pull(keywords) |>
    stringr::str_split("; |,") |>
    unlist() |>
    stringr::str_trim() |>
    unique() |>
    sort()
  
  # Precompute species dataset IDs (split semicolon-separated values)
  message("Computing species dataset IDs...")
  temp_species_ids <- austraits_species_averages |> 
    dplyr::select(dataset_id) |> 
    dplyr::distinct() |> 
    dplyr::collect() |> 
    dplyr::pull(dataset_id)
  
  dropdown_cache$all_dataset_ids_species <- unique(sort(unlist(strsplit(temp_species_ids, "; "))))
  
  saveRDS(dropdown_cache, file.path(output_dir, "dropdown_cache.rds"))
  message("✓ Dropdown cache saved")

  # APC synonyms for Taxon View (needs APCalign)
  prepare_synonyms_for_portal(output_dir)
}

#' Prepare the published species means for the portal
#'
#' @description Keeps every column of the published observed means, so the
#'   portal's table and downloads match the release's dictionary
#'   (`austraits-<version>-observed-means-dictionary.csv`), and adds only the
#'   columns the portal needs:
#'   - `source_primary_key` and `source_primary_citation`: each contributing
#'     dataset's own reference, `"; "`-joined, for the Citations tab and
#'     downloads (the raw data cite datasets the same way);
#'   - `taxon_distribution`, from the database's taxa table, for the APC
#'     distribution filter. A species formed by rolling up infraspecific
#'     names, with no row of its own in the taxa table, has none;
#'   - `row_id`, which pages and downloads rely on.
#' @param means Published observed means (one row per taxon and trait).
#' @param database The traits.build database the means were built from.
#' @param flattened The flattened database, as written to
#'   `austraits-data.parquet` (for the citations).
#' @return The means with the added columns.
#' @keywords internal
#' @noRd
prepare_species_means <- function(means, database, flattened) {
  required <- c("taxon_name", "trait_name", "unit", "type", "value", "value_min",
                "value_max", "value_mode", "value_count", "n_records", "dataset_id",
                "genus", "family")
  missing <- setdiff(required, names(means))
  if (length(missing) > 0) {
    stop("The species means lack columns the portal needs: ", paste(missing, collapse = ", "),
         ". Are these the published observed means?", call. = FALSE)
  }

  citations <- flattened |>
    dplyr::distinct(source_primary_key, source_primary_citation) |>
    dplyr::filter(!is.na(source_primary_key))
  citations <- citations[!duplicated(citations$source_primary_key), ]

  # One citation string per distinct dataset_id combination
  dataset_sets <- unique(means$dataset_id)
  keys <- strsplit(dataset_sets, "; ", fixed = TRUE)
  joined_citations <- vapply(keys, function(k) {
    paste(citations$source_primary_citation[match(k, citations$source_primary_key)], collapse = "; ")
  }, character(1))

  distribution <- database$taxa |>
    dplyr::select("taxon_name", "taxon_distribution") |>
    dplyr::distinct(taxon_name, .keep_all = TRUE)

  out <- means |>
    dplyr::mutate(
      # Hybrid formulas starting "(Genus species x ...)" get the genus
      # "(Genus" upstream (APCalign); strip the bracket (issue #113)
      genus = stringr::str_remove(genus, "^\\("),
      source_primary_key = dataset_id,
      source_primary_citation = unname(joined_citations[match(dataset_id, dataset_sets)])
    ) |>
    dplyr::left_join(distribution, by = "taxon_name") |>
    dplyr::mutate(row_id = dplyr::row_number())

  stopifnot(nrow(out) == nrow(means))
  n_no_distribution <- sum(is.na(out$taxon_distribution))
  if (n_no_distribution > 0) {
    message(sprintf("  %s of %s species-mean rows have no APC distribution (taxa not in the taxa table, or none recorded)",
                    format(n_no_distribution, big.mark = ","), format(nrow(out), big.mark = ",")))
  }
  out
}

#' The release record for a database
#'
#' @description The version and DOIs the portal shows in its text and writes
#'   into every download. The version comes from the database itself
#'   (`build_info$version`); its DOI from the Zenodo versions of the concept
#'   record. A version with no Zenodo record (a development build such as
#'   the lite set) gets `doi = NA`.
#' @param database A traits.build database.
#' @param versions A table of versions (`version`, `doi`, `publication_date`),
#'   as returned by `traits.use::list_versions()`; fetched if NULL.
#' @return A list with `version`, `doi`, `concept_doi`, `publication_date` and
#'   `austraits_package` (the austraits package tag used to prepare the data,
#'   or NA).
#' @keywords internal
#' @noRd
portal_release_info <- function(database, versions = NULL) {
  version <- sub("^v", "", database$build_info$version)
  concept_doi <- database$metadata$doi
  if (is.null(versions)) {
    versions <- tryCatch(traits.use::list_versions(concept_doi), error = function(e) {
      warning("Could not list the Zenodo versions (", conditionMessage(e), "); the release DOI is left empty",
              call. = FALSE)
      NULL
    })
  }
  match <- if (is.null(versions)) integer() else which(sub("^v", "", versions$version) == version)
  austraits_package <- if (requireNamespace("austraits", quietly = TRUE)) {
    paste0("v", utils::packageVersion("austraits"))
  } else {
    NA_character_
  }
  list(
    version = version,
    doi = if (length(match) == 1) versions$doi[match] else NA_character_,
    concept_doi = concept_doi,
    publication_date = if (length(match) == 1) as.character(versions$publication_date[match]) else NA_character_,
    austraits_package = austraits_package
  )
}

#' Join each dataset's own reference to the traits table
#'
#' @description The portal cites each dataset by its own reference, as
#'   `source_primary_key` and `source_primary_citation`. `flatten_database()`
#'   does not add these: databases built with traits.build 2.3 or later drop
#'   `source_primary_key` (always the `dataset_id` there) and call the citation
#'   `dataset_id_citation`. Older databases keep both columns in `methods`, and
#'   there the key can differ from the `dataset_id` (Westoby_2014 cites
#'   Gleason_2012), so it is taken from `methods` as it stands.
#' @param database A traits.build database
#' @return The database, with the two columns added to its traits table
#' @keywords internal
#' @noRd
join_primary_source <- function(database) {
  if (all(c("source_primary_key", "source_primary_citation") %in% names(database$methods))) {
    return(traits.use::join_methods(database, vars = c("source_primary_key", "source_primary_citation")))
  }
  if (!"dataset_id_citation" %in% names(database$methods)) {
    stop("The methods table has neither `source_primary_key`/`source_primary_citation` ",
         "(traits.build before 2.3) nor `dataset_id_citation` (2.3 and later), ",
         "so the portal cannot cite each dataset.", call. = FALSE)
  }
  database <- traits.use::join_sources(database, vars = "dataset_id_citation")
  database$traits <- database$traits |>
    dplyr::mutate(source_primary_key = dataset_id) |>
    dplyr::rename(source_primary_citation = "dataset_id_citation")
  database
}

#' Format flattened database for display
#' 
#' @param database Flattened traits.build object
#' @return Formatted database
#' @keywords internal 
#' @noRd 
#' @importFrom tidyselect ends_with starts_with
format_database_for_display <- function(database) {
  database |> 
    dplyr::select(
      -c(ends_with("_id")),
      -starts_with("source"),
      # Reference columns from traits.use's join_sources(); the portal shows
      # source_primary_citation instead
      -tidyselect::any_of(c("dataset_id_type", "dataset_id_citation", "secondary_citation")),
      -c("methods",
         "description", 
         "assistants", 
         "dataset_curators", 
         "sampling_strategy"),
      "dataset_id", 
      "source_id", # The reference each observation was taken from
      "source_primary_citation", 
      "source_primary_key", # For usage text
      "location_id", # For trait profile
      "observation_id", # For trait profile,
      "taxon_name", "taxon_distribution", "taxon_rank":"aligned_name_taxonomic_status", # For trait profile
      "row_id"
    ) |> 
    # An empty source_id means the dataset's own reference: traits.build 2.3
    # fills it with the dataset_id, and traits.use's join_sources() reads it so
    # for older databases
    dplyr::mutate(source_id = dplyr::coalesce(source_id, dataset_id)) |>
    dplyr::relocate("dataset_id", .before = "taxon_name") |> 
    dplyr::relocate("source_id", .after = "dataset_id") |> 
    dplyr::relocate("source_primary_citation", .after = "method_context_properties") |> 
    dplyr::relocate(c("genus", "family"), .after = "taxon_name") |>
    dplyr::arrange(family, taxon_name, trait_name)
}
