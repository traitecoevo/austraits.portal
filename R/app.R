#' Open AusTraits Data Portal Locally
#' @export
#' @param ... arguments passed to ShinyApp()

open_data_portal <- function(...){
  # Run the application 
  shiny::shinyApp(ui = austraits_ui, server = austraits_server, ...)
}