
#' @title Austraits weighted means
#' @description
#' This function calculates the weighted means for Austraits data.  
#' It takes a data frame of Austraits data and a vector of trait names as input.
#' It filters the data for the specified traits and value types,  

estimate_species_trait_means_numerical <- function(austraits, traits) {

  # any data that is a mean, median or raw, create a site mean
  location_means <- austraits |> estimate_species_trait_means_locations(traits)

  # any data that is a max or a min (range) and basically from a flora, create a mean value
  flora_means <- austraits |> estimate_species_trait_means_floras(traits)
  
  # combine the two, then take means across site and flora replicates
  means <- location_means |>
    dplyr::bind_rows(flora_means)  |>
    dplyr::group_by(taxon_name, trait_name) |>
    dplyr::summarise(.groups = "drop",
      value_mean = mean(value_mean),
      value_min = min(value_min),
      value_max = max(value_max),
      value_median = median(value_median),
      value_geom_mean = 10^mean(log10(value_mean)),
      all_replicates = sum(all_replicates),
      location_replicates = sum(location_replicates),
      flora_replicates = sum(flora_replicates),
      # record sources
      dataset_id = paste(unique(dataset_id), collapse = "; "),
      observation_id = paste(unique(observation_id), collapse = "; "),
    ) |>
    dplyr::distinct()
  
  means
}

estimate_species_trait_means_locations <- function(austraits, traits) {
  # any data that is a mean, median or raw, create a location mean
  austraits |>
  dplyr::filter(
    trait_name %in% traits,
    value_type %in% c("mean", "raw", "median")
  ) |>
  dplyr::mutate(
    value = as.numeric(value),
    type = "site",
    replicates = 1,
    log10_value = log10(value)
  ) |>
  dplyr::group_by(taxon_name, trait_name, type, dataset_id, location_id) |>
  dplyr::summarise(
    .groups = "drop",
    dplyr::across(value, list(mean = mean, min = min, max = max, median = median), na.rm = TRUE),
    dplyr::across(c("latitude (deg)", "longitude (deg)", "location_name"), dplyr::first),
    all_replicates = sum(replicates),
    value_geom_mean = 10^mean(log10_value),
    observation_id = paste(unique(observation_id), collapse = "; "),
  ) |>
  # dplyr::distinct(taxon_name, trait_name, dataset_id, observation_id, location_id, value_mean, value_min, value_max, value_median, value_geom_mean, all_replicates, location_name, `latitude (deg)`, `longitude (deg)`) |>
  dplyr::mutate(
    location_replicates = 1,
    flora_replicates = 0
  )
}

estimate_species_trait_means_floras <- function(austraits, traits) {
  flora_data <- 
    austraits |>
    dplyr::filter(
      trait_name %in% traits,
      value_type %in% c("minimum", "maximum"), 
      basis_of_record %in% c("preserved_specimen", "literature"))
  
  # earky exit if no data available
  if(nrow(flora_data) == 0) { return(flora_data)}
  
  flora_data |>
  dplyr::mutate(
    value = as.numeric(value),
    type = "flora",
  )|>
  dplyr::group_by(taxon_name, trait_name, type, dataset_id, observation_id, original_name) |> 
  dplyr::summarise(.groups = "drop",
    dplyr::across(value, list(mean = mean, min = min, max = max), na.rm = TRUE), 
  ) |>
#  dplyr::distinct(taxon_name, trait_name, dataset_id, observation_id, original_name, value_mean, value_min, value_max) |>
  dplyr::mutate(
    location_replicates = 0,
    flora_replicates = 1,
    all_replicates = 1,
    value_median = value_mean
  )

}


estimate_species_trait_summary_categorical <- function(austraits, trait_names) {

  austraits |>
  estimate_species_trait_value_summary_categorical(trait_names) |>
  dplyr::mutate(
    tmp_summary = paste0(value, " (", replicates, ")")
  ) |>
  dplyr::group_by(taxon_name, trait_name) |>
  dplyr:: mutate(
      value = paste0(tmp_summary, collapse = "; ")
    ) |>
  dplyr::ungroup() |>
  dplyr::select(-dplyr::all_of(c("tmp_summary"))) |>
  dplyr::distinct()
}

estimate_species_trait_value_summary_categorical <- function(austraits, trait_names) {

  austraits |>
  dplyr::filter(trait_name %in% trait_names) |>
  dplyr::select(dplyr::all_of(c("dataset_id", "taxon_name", "trait_name", "location_id", "observation_id", "value")))|>
  dplyr::mutate(value = stringr::str_split(value, " ")) |>
  tidyr::unnest_longer(value) |>
  dplyr::mutate(
    replicates = 1
  ) |>
  dplyr::group_by(taxon_name, trait_name, value) |>
  summarise(.groups = "drop",
    value = first(value),
    replicates = sum(replicates),
    dataset_id = paste(unique(dataset_id), collapse = "; "),
    observation_id = paste(unique(observation_id), collapse = "; ")
  )
}
