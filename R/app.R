#' Open AusTraits Data Portal Locally
#' @export
#' @param ... arguments passed to ShinyApp()

open_data_portal <- function(...){
  # Download data
  # This is not currently used
  # retrieve_github_release_parquet(version_tag = "6.0.0", 
  #                                output_dir = file.path(system.file("extdata/austraits", package = "austraits.portal"))) 

  # Run the application 
  shiny::shinyApp(ui = austraits_ui, server = austraits_server, ...)
}
