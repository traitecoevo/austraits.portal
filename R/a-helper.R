#' Check if input has filters

has_input_value <- function(input, input_name) {
  !is.null(input[[input_name]]) && length(input[[input_name]]) > 0
}

#' Determine valid filters in the input list

valid_filters <- function(input, exclude_taxon_rank = TRUE){
  valid_vars <- c(names(austraits))
  
  if(exclude_taxon_rank){
    valid_vars <- valid_vars[valid_vars != "taxon_rank"]
    stringr::str_subset(names(input), paste(valid_vars, collapse = "|"))
  } else
  stringr::str_subset(names(input), paste(valid_vars, collapse = "|"))
}

#' Apply filters
apply_filters <- function(data = austraits, input){
  
  # Exclude user inputs that we can't filter on
  # TODO function to id these
  # TODO need to exclude data_table_ prefixes might be better to 
  valid_filters <- valid_filters(input)
  
  
  # Construct filter conditions dynamically
  filter_conditions <- purrr::map(valid_filters, function(v) {
    value <- input[[v]]
    if (!is.null(value)) {
      expr(.data[[v]] %in% !!value)  # Dynamically create filter expressions
    } else {
      NULL
    }
  }) |> purrr::compact()  # Remove NULL conditions
  
  # browser()

  # Combine all filter conditions into a single filter call
  filtered_parquet <- data |> 
    dplyr::filter(!!!filter_conditions)  # Unquote and splice the conditions
  
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
#' @importFrom tidyselect ends_with starts_with

format_database_for_display <- function(database){
  
#  browser()

  database |> 
    dplyr::select(
      -c(ends_with("_id")),
      -starts_with("source"),
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