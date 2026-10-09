#' Server logic for URL query parameter handling
#' 
#' @description Loads filters from URL on app startup
#' @param input,output,session Shiny session objects
#' @param family_choices Reactive for family choices
#' @param genus_choices Reactive for genus choices
#' @param taxon_name_choices Reactive for taxon name choices
#' @param loading_from_url reactiveVal, TRUE while URL values are being applied
#' @param publish_filter_result Function(parsed_filters, context, log_search)
#'   from app_server that runs the query and publishes the result
#' @param filters Reactive filters from mod_filters_server
#' @param apply_filters_trigger reactiveVal counting applied searches
#' 
#' @noRd
srv_url_params <- function(input, output, session, 
                           family_choices, 
                           genus_choices, 
                           taxon_name_choices,
                           loading_from_url,
                           publish_filter_result,
                           filters,
                           apply_filters_trigger) {
  
  url_processed <- reactiveVal(FALSE)

  observe({
    if (url_processed()) return()
    
    query <- parseQueryString(session$clientData$url_search)
    
    if (length(query) > 0) {
      loading_from_url(TRUE)
      
      isolate({
        # Handle taxon rank first
        if (!is.null(query$taxon_type) && query$taxon_type %in% c("all", "family", "genus", "taxon_name")) {
          updateRadioButtons(session, "filters-taxon_type", selected = query$taxon_type)
        }
        
        # Handle taxon selections WITH CHOICES
        if (!is.null(query$family)) {
          updateSelectizeInput(session, "filters-family", 
                              choices = family_choices(), 
                              selected = query$family, 
                              server = TRUE)
        }
        
        if (!is.null(query$genus)) {
          updateSelectizeInput(session, "filters-genus", 
                              choices = genus_choices(), 
                              selected = query$genus, 
                              server = TRUE)
        }
        
        if (!is.null(query$taxon_name)) {
          updateSelectizeInput(session, "filters-taxon_name", 
                              choices = taxon_name_choices(), 
                              selected = query$taxon_name, 
                              server = TRUE)
        }
        
        # Handle trait name WITH CHOICES
        if (!is.null(query$trait_name)) {
          updateSelectizeInput(session, "filters-trait_name", 
                              choices = dropdowns$all_traits, 
                              selected = query$trait_name, 
                              server = TRUE)
        }
        
        # Handle location filters
        if (!is.null(query$location) && query$location %in% c("georeferenced", "apc")) {
          updateRadioButtons(session, "filters-location", selected = query$location)
        }
        
        if (!is.null(query$apc_taxon_distribution)) {
          states <- strsplit(query$apc_taxon_distribution, ",")[[1]]
          updateSelectizeInput(session, "filters-apc_taxon_distribution", 
                              choices = c("ACT", "NSW", "NT", "Qld", "SA", "Tas", "Vic", "WA"),
                              selected = states, 
                              server = TRUE)
        }
        
        # Handle basis of record WITH CHOICES
        if (!is.null(query$basis_of_record)) {
          updateSelectizeInput(session, "filters-basis_of_record", 
                              choices = dropdowns$all_bor, 
                              selected = query$basis_of_record, 
                              server = TRUE)
        }
        
        # Handle life stage WITH CHOICES
        if (!is.null(query$life_stage)) {
          updateSelectizeInput(session, "filters-life_stage", 
                              choices = dropdowns$all_age, 
                              selected = query$life_stage, 
                              server = TRUE)
        }
        
        # Handle tab switching
        if (!is.null(query$tab)) {
          tab_name <- utils::URLdecode(query$tab)
          updateNavbarPage(session, "main_tabs", selected = tab_name)
        }
      })
      
  # Keep the guard up while the browser applies the updates above, without
  # blocking the R process (a Sys.sleep() here stalled every user for 2 s)
  later::later(function() loading_from_url(FALSE), delay = 2)
  url_processed(TRUE)

  parsed_filters <- list(
    dataset_type = "species",
    taxon = list(
      taxon_type = if (!is.null(query$taxon_type)) query$taxon_type else "all",
      family = if (!is.null(query$family)) query$family else NULL,
      genus = if (!is.null(query$genus)) query$genus else NULL,
      taxon_name = if (!is.null(query$taxon_name)) query$taxon_name else NULL
    ),
    trait = list(
      trait_filter_type = "name",
      trait_name = if (!is.null(query$trait_name)) query$trait_name else NULL,
      trait_grouping = NULL,
      structure_measured = NULL,
      keywords = NULL
    ),
    location = list(
      location = if (!is.null(query$location)) query$location else NULL,
      apc_taxon_distribution = if (!is.null(query$apc_taxon_distribution)) {
        strsplit(query$apc_taxon_distribution, ",")[[1]]
      } else NULL,
      min_latitude = NULL,
      max_latitude = NULL,
      min_longitude = NULL,
      max_longitude = NULL
    ),
    other = list(
      basis_of_record = if (!is.null(query$basis_of_record)) query$basis_of_record else NULL,
      life_stage = if (!is.null(query$life_stage)) query$life_stage else NULL
    ),
    custom = list(),
    has_filters = !is.null(query$family) || !is.null(query$genus) || 
                  !is.null(query$taxon_name) || !is.null(query$trait_name)
  )      
      publish_filter_result(parsed_filters, "Filters from URL", log_search = TRUE)

      if (isTRUE(parsed_filters$taxon$taxon_type == "taxon_name") &&
          length(parsed_filters$taxon$taxon_name) == 1) {

        target_taxon <- parsed_filters$taxon$taxon_name
        taxon_view_unlocked <- reactiveVal(FALSE)

        observe({
          if (taxon_view_unlocked()) return()
          fv <- filters()
          if (isTRUE(fv$taxon_type == "taxon_name") &&
              isTRUE(identical(fv$taxon_name, target_taxon))) {
            taxon_view_unlocked(TRUE)
            apply_filters_trigger(apply_filters_trigger() + 1)
          }
        })
      }
    }
  })
}
