#' Check if input has filters

has_input_value <- function(input, input_name) {
  !is.null(input[[input_name]]) && length(input[[input_name]]) > 0
}

#' Apply filters
apply_filters <- function(data = austraits, input){
  # Exclude user inputs that we can't filter on
  valid_filters <- setdiff(names(input), 
                           c("clear_filters", "taxon_rank", "location", "user_coordinates", "user_state", "user_APC_state")
                           )
  
  
  # Construct filter conditions dynamically
  filter_conditions <- purrr::map(names(valid_filters), function(i) {
    value <- input[[i]]
    if (!is.null(value)) {
      expr(.data[[i]] %in% !!value)  # Dynamically create filter expressions
    } else {
      NULL
    }
  }) |> purrr::compact()  # Remove NULL conditions
  
  # browser()

  # Combine all filter conditions into a single filter call
  filtered_parquet <- data |> 
    filter(!!!filter_conditions)  # Unquote and splice the conditions
  
  return(filtered_parquet)
}



#' Find distinct values for a given variable
#' @keywords internal
extract_distinct_values <- function(data, var_name){
  data |> 
    dplyr::distinct({{var_name}}) |> 
    dplyr::filter(!is.na({{var_name}})) |> 
    dplyr::arrange({{var_name}}) |> 
    dplyr::collect() |> 
    dplyr::pull()
}

#' Format relational database for display
#'
#' @param database traits.build object

format_database_for_display <- function(database){
  
  browser()
  
  dplyr::select(
    -dplyr::ends_with("_id"),
    -dplyr::starts_with("source"),
    -c("methods",
       "description", 
       "assistants", 
       "dataset_curators", 
       "aligned_name",
       "binomial", 
       "trinomial", 
       "taxon_name_alternatives", 
       "sampling_strategy"),
    "dataset_id", 
    "source_primary_citation"
  ) |> 
    dplyr::relocate("dataset_id", .before = "taxon_name") |> 
    dplyr::relocate("source_primary_citation", .after = "method_context_properties") |> 
    dplyr::relocate(c("genus", "family"), .after = "taxon_name") 
}


#' Format database for download handler
#'
#' @param database traits.build object

format_database_for_download <- function(database){
    dplyr::select(-dplyr::ends_with(".x")) |> 
    dplyr::rename(genus = "genus.y",
           family = "family.y",
           taxon_rank = "taxon_rank.y",
           establishment_means = "establishment_means.y") 
}