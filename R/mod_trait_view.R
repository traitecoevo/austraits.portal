#' trait_view UI Function
#'
#' @description A shiny Module for displaying trait profiles
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_trait_view_ui <- function(id){
  ns <- NS(id)
  
  card(
    card_header("AusTraits trait profile"),
    card_body(
      card(
        htmlOutput(ns("trait_profile")),
        min_height = 600
      ),
      card(
        card_header("Observed values"),
        card_body(
          uiOutput(ns("trait_histogram_text")),
          plotly::plotlyOutput(ns("trait_histogram_plot"))
        ),
        min_height = 650,
        full_screen = TRUE,
        fillable = FALSE
      ),
      # Conditionally show map only for raw data
      uiOutput(ns("trait_geo_card"))
    )
  )
}
    
#' trait_view Server Functions
#'
#' @param id Internal parameter for {shiny}
#' @param filtered_data Reactive containing filtered data
#' @param filters Reactive containing filter values
#' @param main_tabs Reactive containing the active tab name
#'
#' @noRd 
mod_trait_view_server <- function(id, filtered_data, filters, main_tabs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
# Check if current dataset is species averages
    is_species_avg <- reactive({
      req(filters())
      # Use dataset_type from filters instead of checking columns
      filters()$dataset_type == "species"
    })
    
    # The filtered rows for the selected trait, collected once and shared by
    # the profile, the plot and the map. NULL unless exactly one trait is in
    # the result: counting distinct traits in DuckDB is cheap, whereas
    # collecting first cost ~4 s and 1 GB on unfiltered raw data, only to
    # show "Please select a single trait".
    trait_data <- reactive({
      req(main_tabs() == "Trait View")
      query <- filtered_data()
      req(query)

      n_traits <- query |>
        dplyr::summarise(n = dplyr::n_distinct(trait_name)) |>
        dplyr::collect() |>
        dplyr::pull(n)
      if (n_traits != 1) return(NULL)

      start_time <- Sys.time()
      data <- query |> dplyr::collect()
      cat("[TRAIT VIEW] Collected", nrow(data), "rows in",
          round(as.numeric(Sys.time() - start_time, units = "secs"), 3), "seconds\n")
      data
    })

    trait_profile <- reactive({
      req(main_tabs() == "Trait View")
      full_data <- trait_data()

      if (is.null(full_data)) {
        return(list(tags$p("Please select a single trait to view."), NULL, "", leaflet::leaflet()))
      }

      if (is_species_avg()) {
        # Add ALL missing columns for species dataset
        data_with_location <- full_data |>
          dplyr::mutate(
            `latitude (deg)` = NA_real_,
            `longitude (deg)` = NA_real_,
            # Add value column from mean_value (for plot compatibility)
            value = if ("mean_value" %in% names(full_data)) mean_value else NA_real_,
            # Add value_type if missing
            value_type = if (!"value_type" %in% names(full_data)) "mean" else value_type,
            # Add unit if missing
            unit = if (!"unit" %in% names(full_data)) NA_character_ else unit
          )
        
        # Generate full profile (works now with dummy location)
        full_profile <- tryCatch({
          generate_trait_profile(data_with_location)
        }, error = function(e) {
          # Fallback if still fails
          return(list(tags$p("Trait profile unavailable"), NULL, NULL, NULL))
        })
        
        # Add species averages banner at top 
        banner <- tags$div(
          style = "background: #f2f6f3; border-left: 4px solid #2f7d55; padding: 12px 16px; margin-bottom: 5px; border-radius: 4px;",
          tags$div(
            style = "display: flex; align-items: center; gap: 8px;",
            icon("info-circle", style = "color: #2f7d55;"),
            tags$span(
              style = "color: #235c43; font-weight: 500;",
              "Species Averages Dataset"
            )
          ),
          tags$p(
            style = "margin: 8px 0 0 0; font-size: 0.9em; color: #424242;",
            "Showing aggregated species-level means. Location data not available."
          ),
          tags$p(
            style = "margin: 8px 0 0 0; font-size: 0.85em; color: #616161; font-style: italic;",
            "To view individual observations, select 'Raw data' in the filters panel."
          )
        )
        
        # Combine banner with trait info
        trait_info_with_banner <- tagList(banner, full_profile[[1]])
        
        # Return with banner, skip location/map
        return(list(trait_info_with_banner, NULL, NULL, NULL))
      }
      
      raw_profile <- generate_trait_profile(full_data)
      
      # raw data banner
      banner <- tags$div(
        style = "background: #f2f6f3; border-left: 4px solid #2f7d55; padding: 12px 16px; margin-bottom: 5px; border-radius: 4px;",
        tags$div(
          style = "display: flex; align-items: center; gap: 8px;",
          icon("info-circle", style = "color: #2f7d55;"),
          tags$span(
            style = "color: #235c43; font-weight: 500;",
            "Raw Observation Data"
          )
        ),
        tags$p(
          style = "margin: 8px 0 0 0; font-size: 0.9em; color: #424242;",
          "Showing individual observations with full location and measurement details."
        ),
        tags$p(
          style = "margin: 8px 0 0 0; font-size: 0.85em; color: #616161; font-style: italic;",
          "To view species-level aggregated means, select 'Species averages' in the filters panel."
        )
      )
      
      trait_info_with_banner <- tagList(banner, raw_profile[[1]])
      
      return(list(trait_info_with_banner, raw_profile[[2]], raw_profile[[3]], raw_profile[[4]]))
    })
    
    output$trait_profile <- renderUI({
      tagList(trait_profile()[[1]])
    })
    
    output$trait_histogram_text <- renderUI({
      if (is_species_avg()) {
        tagList(
          p("The plot below shows the distribution of species-level mean values for this trait."),
          p("Each point represents the mean trait value for a species, calculated from all available observations for that species.")
        )
      } else {
        tagList(
          p("The plot below shows the distribution of selected data for this trait, including data collected on individuals of all age classes (seedling, sapling, adult), both field-collected and experimental data, and data representing individuals and population means."),
          p("Visualising data records across the families with the most data for the trait indicates the taxonomic breadth of information for this trait")
        )
      }
    })
    
    output$trait_histogram_plot <- plotly::renderPlotly({
      data <- trait_data()
      req(data)
      
      # For species averages, use value_mean instead of value
      if (is_species_avg()) {
        data <- data |>
          dplyr::mutate(value = value_mean)
      }
      
      # Calculate dynamic height based on number of families
      num_families <- length(unique(data$family))
      # Min 400px, add 15px per family after first 20
      plot_height <- max(400, 300 + (num_families * 15))
      
      plot_trait_distribution(data, data$trait_name[1]) |>
        plotly::ggplotly(tooltip = c("x", "y", "text"), height = plot_height)
    })
    
    # Conditionally render geography card
    output$trait_geo_card <- renderUI({
      if (is_species_avg()) {
        # Hiding map for species averages
        return(NULL)
      } else {
        # Show map for raw data
        card(
          card_header("Geographical distribution of trait data"),
          min_height = 600,
          card_body(
            uiOutput(ns("trait_geo_text")),
            leaflet::leafletOutput(ns("trait_geo_map"), height = "600px")
          )
        )
      }
    })
    
    output$trait_geo_text <- renderUI({
      if (!is_species_avg()) {
        trait_profile()[[3]]
      }
    })
    
    output$trait_geo_map <- leaflet::renderLeaflet({
      if (!is_species_avg()) {
        trait_profile()[[4]]
      }
    })
  })
}
    
## To be copied in the UI
# mod_trait_view_ui("trait_view_1")
    
## To be copied in the server
# mod_trait_view_server("trait_view_1")
