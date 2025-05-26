#' Check if input has filters
#' @keywords internal

has_input_value <- function(input, input_name) {
  !is.null(input[[input_name]]) && length(input[[input_name]]) > 0
}

#' Determine valid filters in the input list
#' @keywords internal


valid_filters <- function(input, exclude_taxon_rank = TRUE){
  
  # Get the names of the input variables
  v <- names(input)
  # Limit to values in the database
  v <- v[v %in% names(austraits)]
  # remove null values
  v <- v[purrr::map_lgl(v, ~ !is.null(input[[.x]]))]
  # remove taxon rank if requested
  if (exclude_taxon_rank) {
    v <- v[v != "taxon_rank"]
  }

  v
}

#' Apply filters
#' @keywords internal
apply_filters_categorical <- function(data = austraits, input){
  
  # Generate a list of filter conditions based on the input
  filter_conditions <-
    input |>
    valid_filters() |>
    # Construct filter conditions dynamically
    purrr::map(function(v) {
      # paste in case value contains more than one item
      value <- input[[v]] |> paste(collapse = "|")
      # only match whole cells
      value <- paste0("^(", value, ")$")
      # formulate the filter condition as an expression
      expr(stringr::str_detect(.data[[v]], !!value))
    })

  # Combine all filter conditions into a single filter call
  data <- data |>
    # Apply the filter conditions to the data
    dplyr::filter(!!!filter_conditions)  # Unquote and splice the conditions
  
  return(data)
}

#' Apply location filters
#' @param data The data to filter
#' @param input The input list containing filter values
#' @return The filtered data. By default, the entire database is returned.
#' @keywords internal
#' @noRd 
apply_filters_location <- function(data = austraits, input){
  
  if(is.null(input$location)) {
    return(data)
  }

  # georeferenced data: retrieve rows with valid latitude and longitude
  if (input$location == "georeferenced") {
    data <- data |> 
      dplyr::filter(
        !is.na(.data$`latitude (deg)`) & !is.na(.data$`longitude (deg)`)
      )
  } 

  # Taxon distribution: filter for any of the selected states
  if (input$location == "apc" && !is.null(input$apc_taxon_distribution)) {  
    apc_states_selected <- paste(input$apc_taxon_distribution, collapse = "|")

    data <- data |> 
      dplyr::filter(
        stringr::str_detect(.data$taxon_distribution, apc_states_selected)
      )
  }

  return(data)
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

#' Format flattened database for display
#' @keywords internal 
#' @noRd 
#' @param database flattened traits.build object
#' @importFrom tidyselect ends_with starts_with

format_database_for_display <- function(database){
  
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
      -c("taxon_id_genus",
         "taxon_id_family",
         "taxonomic_resolution",
         "aligned_name_taxonomic_status",
         "taxonomic_status",
         "scientific_name",
         "taxonomic_dataset"),
      "dataset_id", 
      "source_primary_citation", 
      "source_primary_key",
      "row_id"
    ) |> 
    dplyr::relocate("dataset_id", .before = "taxon_name") |> 
    dplyr::relocate("source_primary_citation", .after = "method_context_properties") |> 
    dplyr::relocate(c("genus", "family"), .after = "taxon_name")
}

#' Format hyperlinks in flattened database for display
#' @keywords internal 
#' @noRd
#' @param database flattened traits.build object

format_hyperlinks_for_display <- function(database){
  database |> 
  dplyr::mutate(
    source_primary_citation_URL = stringr::str_match(.data$source_primary_citation, "\\((https?://[^\\s)]+)\\)")[,2], # Extract URL
    source_primary_citation = gsub("\\[([^]]+)\\]\\([^)]+\\)", "\\1", .data$source_primary_citation), # Remove DOI MD link structure
    source_primary_citation = gsub("_([^_]+)_", "<i>\\1</i>", .data$source_primary_citation), # Replace MD italics with HTML italics
    source_primary_citation = paste0('<a href="', .data$source_primary_citation_URL, '" target="_blank">', .data$source_primary_citation, '</a>') # Replaces the original source_primary_citation with a truncated HTML version
  ) |>
  dplyr::select(
    -"source_primary_citation_URL"
  ) 
}

#' Retrieve all assets from a GitHub Release
#'
#' @param version_tag The version tag of the GitHub release (e.g., "6.0.0")
#' @param output_dir The local directory to save the downloaded files
#' @return A vector of paths to the downloaded files
#' @export

retrieve_github_release_parquet <- function(version_tag = "6.0.0", output_dir = system.file("extdata/austraits", package = "austraits.portal")) {
  
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Construct the URL for the GitHub release API
  api_url <- paste0("https://api.github.com/repos/traitecoevo/austraits.portal/releases/tags/", version_tag)
  
    # Make a GET request to fetch release information
  response <- tryCatch(
    httr2::request(api_url) |> 
      httr2::req_perform(),
    error = function(e) {
      warning("Failed to fetch release information: ", e$message)
      return(NULL)
    }
  )
  
  # If the response is NULL, return early
  if (is.null(response)) {
    return(NULL)
  }
  
  # Check if the request was successful
  if (httr2::resp_status(response) != 200) {
    warning("Failed to fetch release information. HTTP status: ", httr2::resp_status(response))
    return(NULL)
  }
  
  # Parse the response body as JSON
  release_info <- httr2::resp_body_json(response)
  
  # Extract asset information
  assets <- release_info$assets
  if (length(assets) == 0) {
    stop("No assets found in the release.")
  }
  
  # Download parquet only 
  downloaded_files <- 
    asset_url <- assets[[1]]$browser_download_url
    file_name <- assets[[1]]$name
    output_path <- file.path(output_dir, file_name)
    
  # Check if the file already exists
  if (file.exists(output_path)) {
    message("Asset already exists: ", output_path)
    return(output_path)
  }

    # Download the asset
    asset_response <- httr2::request(asset_url) |>
      httr2::req_perform()
    
    # Check if the request was successful
    if (httr2::resp_status(asset_response) != 200) {
      stop("Failed to download asset: ", file_name, ". HTTP status: ", httr2::resp_status(asset_response))
    }
    
    # Write the content to the specified output path
    writeBin(httr2::resp_body_raw(asset_response), output_path)
    message("Asset downloaded successfully: ", output_path)
    return(output_path)
}
