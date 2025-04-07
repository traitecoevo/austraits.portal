#' Apply filters

apply_filters <- function(data = austraits, list = input){
  # Convert to workable list
  input_list <- reactiveValuesToList(input)
  
  # Drop empty lists and clear_filter
  active_inputs <- input_list |> 
    purrr::discard(is.null) |> 
    purrr::discard_at("clear_filters")
  
  # Clean up input names
  stringr::str_remove(names(active_inputs), "user_")
    
  
  for(i in seq_along(active_inputs)) {
    df_filtered <- austraits |> 
    dplyr::filter(.data[[input[[i]]]] %in% list[[i]]$value)
}

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
  flatten_database(database) |> 
    select(-ends_with(".x"),
           -ends_with("_id"),
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
           "source_primary_citation") |> 
    rename(genus = "genus.y",
           family = "family.y",
           taxon_rank = "taxon_rank.y",
           establishment_means = "establishment_means.y") |> 
    relocate("dataset_id", .before = "taxon_name") |> 
    relocate("source_primary_citation", .after = "method_context_properties")
}


#' Format database for download handler
#'
#' @param database traits.build object

format_database_for_download <- function(database){
  flatten_database(database) |>
    select(-ends_with(".x")) |> 
    rename(genus = "genus.y",
           family = "family.y",
           taxon_rank = "taxon_rank.y",
           establishment_means = "establishment_means.y") 
}