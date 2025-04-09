#' Apply filters

apply_filters <- function(data = austraits, list = input){
  # Convert to workable list
  input_list <- reactiveValuesToList(input)
  
  # Construct filter conditions dynamically
  filter_conditions <- purrr::map(names(test_input), function(i) {
    value <- input_list[[i]]
    if (!is.null(value)) {
      expr(.data[[i]] %in% !!value)  # Dynamically create filter expressions
    } else {
      NULL
    }
  }) |> purrr::compact()  # Remove NULL conditions
  
  # Combine all filter conditions into a single filter call
  filtered_parquet <- austraits |> 
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