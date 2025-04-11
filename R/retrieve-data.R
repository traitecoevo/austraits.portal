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
  response <- httr2::request(api_url) %>%
    httr2::req_perform()
  
  # Check if the request was successful
  if (httr2::resp_status(response) != 200) {
    stop("Failed to fetch release information. HTTP status: ", httr2::resp_status(response))
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
