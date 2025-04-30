#' Check if input has filters
#' @keywords internal

has_input_value <- function(input, input_name) {
  !is.null(input[[input_name]]) && length(input[[input_name]]) > 0
}

#' Determine valid filters in the input list
#' @keywords internal


valid_filters <- function(input, exclude_taxon_rank = TRUE){
  valid_vars <- c(names(austraits))
  
  if(exclude_taxon_rank){
    valid_vars <- valid_vars[valid_vars != "taxon_rank"]
    stringr::str_subset(names(input), paste(valid_vars, collapse = "|"))
  } else
  stringr::str_subset(names(input), paste(valid_vars, collapse = "|"))
}

#' Apply filters
#' @keywords internal
apply_filters <- function(data = austraits, input){
  
  # Exclude user inputs that we can't filter on
  # TODO need to exclude data_table_ prefixes too if column filters enabled
  valid_filters <- valid_filters(input)
  
  # Construct filter conditions dynamically
  filter_conditions <- purrr::map(valid_filters, function(v) {
    value <- input[[v]]
    if (!is.null(value)) {
      expr(stringr::str_detect(.data[[v]], !!value))  # Dynamically create filter expressions
    } else {
      NULL
    }
  }) |> purrr::compact()  # Remove NULL conditions

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
#' @keywords internal 
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
