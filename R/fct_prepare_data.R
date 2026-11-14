#' Prepare AusTraits data for the portal
#'
#' @description Flattens the AusTraits database, saves definitions and sources, 
#'   and writes parquet files for data and display
#' @param austraits The AusTraits database object
#' @param output_dir Directory to write output files
#' @param overwrite Logical; overwrite existing files if TRUE
#' @keywords internal
#' @noRd
prepare_data_for_portal <- function(austraits, output_dir, overwrite = FALSE) {
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

  # create species means dataset
  # list of traits to take means for - core traits only
  message("Computing species averages for core traits...")
  trait_groups <- readr::read_csv(
    system.file("extdata/austraits/trait_groups_for_portal.csv", package = "austraits.portal"),
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  core_traits <- 
    trait_groups |>
    dplyr::filter(!is.na(core_trait)) |>
    dplyr::pull(trait)

  austraits_species_averages <-
    austraits_full_flatten |>
    dplyr::filter(trait_name %in% core_traits) |>    
    estimate_species_trait_means()
  message("✓ Species averages computed")

  # Save the flattened database
  message("Writing parquet files...")
  austraits_full_flatten |>
    arrow::write_parquet(filename_data)
  
  # Save the display version of the flattened database
  austraits_full_flatten |>
    format_database_for_display() |>
    arrow::write_parquet(filename_display)
  
  # Save the species averages dataset
  austraits_species_averages |>
    arrow::write_parquet(file.path(output_dir, "austraits-species-averages.parquet"))
  
  # Save the display version of the species averages dataset
  austraits_species_averages_display <- austraits_species_averages
    
  austraits_species_averages_display |>
    arrow::write_parquet(file.path(output_dir, "austraits-species-averages-display.parquet")) 
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
  
  # Save metadata as RDS (faster than JSON)
  message("Saving metadata...")
  
  metatdata <- jsonlite::read_json(system.file("extdata/austraits/austraits.json", package = "austraits.portal"))
  saveRDS(metatdata, file.path(output_dir, "metadata.rds"))
  message("✓ Metadata saved")
  
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
  temp_species_ids <- austraits_species_averages_display |> 
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
