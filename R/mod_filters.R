#' Filters UI Module
#'
#' @description Sidebar filters for the AusTraits portal
#'
#' @param id Shiny module id
#'
#' @noRd
#'
#' @importFrom shiny NS
mod_filters_ui <- function(id) {
  ns <- NS(id)

  sidebar(
    h5("Dataset"),
    radioButtons(
      ns("dataset_type"),
      label = NULL,
      choices = c(
        "Raw data" = "raw",
        "Species averages" = "species"
      ),
      selected = "species"
    ),
    
    hr(),

    actionButton(
      ns("apply_filters_btn"),
      "Apply Filters",
      class = "btn-primary w-100 mb-0",
      icon = icon("filter")
    ),

    h5("Taxonomy"),

    radioButtons(
      ns("taxon_type"),
      label = "Filter by which taxon rank:",
      choices = c(
        "All taxa"   = "all",
        "Family"     = "family",
        "Genus"      = "genus",
        "Taxon name" = "taxon_name"
      ),
      selected = "all"
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] == "taxon_name"', ns("taxon_type")),
      selectizeInput(
        ns("taxon_name"),
        label = "Taxon name:",
        choices = NULL,
        multiple = TRUE
      )
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] == "genus"', ns("taxon_type")),
      selectizeInput(
        ns("genus"),
        label = "Genus:",
        choices = NULL,
        multiple = TRUE
      )
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] == "family"', ns("taxon_type")),
      selectizeInput(
        ns("family"),
        label = "Family:",
        choices = NULL,
        multiple = TRUE,
        selected = character(0)
      )
    ),

    h5("Trait"),
    selectizeInput(
      ns("trait_name"),
      label = "Trait name(s):",
      choices = NULL,
      multiple = TRUE,
      options = list(create = TRUE)
    ),

    radioButtons(
      ns("trait_filter_type"),
      label = "Additional trait filters:",
      choices = c(
        "None" = "name",
        "Trait features" = "features"
      ),
      selected = "name"
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] == "features"', ns("trait_filter_type")),
      selectizeInput(
        ns("trait_grouping"),
        label = "Trait grouping:",
        choices = NULL,
        multiple = TRUE
      ),
      
      selectizeInput(
        ns("structure_measured"),
        label = "Structure measured:",
        choices = NULL,
        multiple = TRUE
      ),
      
      selectizeInput(
        ns("keywords"),
        label = "Keywords:",
        choices = NULL,
        multiple = TRUE
      )
    ),

      radioButtons(
        ns("location"),
        label = h5("Location filters:"),
        choices = c(
        "None" = "",
        "Georeferenced records" = "georeferenced",
        "APC taxon distribution" = "apc"
      ),
      selected = ""
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] == "apc"', ns("location")),
      selectizeInput(
        ns("apc_taxon_distribution"),
        label = "State/territory:",
        choices = c("ACT", "NSW", "NT", "Qld", "SA", "Tas", "Vic", "WA"),
        multiple = TRUE
      )
    ),

    conditionalPanel(
      condition = sprintf('input["%s"] == "georeferenced"', ns("location")),
      h6("Filter to bounding box (optional)"),
      numericInput(
        ns("min_latitude"),
        label = "Minimum latitude:",
        value = NA,
        min = -60,
        max = 45
      ),
      numericInput(
        ns("max_latitude"),
        label = "Maximum latitude:",
        value = NA,
        min = -60,
        max = 45
      ),
      numericInput(
        ns("min_longitude"),
        label = "Minimum longitude:",
        value = NA,
        min = -80,
        max = 169
      ),
      numericInput(
        ns("max_longitude"),
        label = "Maximum longitude:",
        value = NA,
        min = -80,
        max = 169
      )
     ),

    h5("Custom Filters"),

    # FILTER 1 - Always visible
    selectizeInput(
      ns("custom_col_1"),
      label = "Filter 1 - Column:",
      choices = NULL,
      multiple = FALSE
    ),

      uiOutput(ns("custom_val_1_ui")),

    # FILTER 2 - Shows when Filter 1 has values
    conditionalPanel(
      condition = sprintf('input["%s"] != "" && input["%s"] != null && input["%s"] != null && input["%s"].length > 0', 
                        ns("custom_col_1"), ns("custom_col_1"), ns("custom_val_1"), ns("custom_val_1")),
      
      selectizeInput(
        ns("custom_col_2"),
        label = "Filter 2 - Column:",
        choices = NULL,
        multiple = FALSE
      ),
      
        uiOutput(ns("custom_val_2_ui"))
    ),

    # FILTER 3 - Shows when Filter 2 has values
    conditionalPanel(
      condition = sprintf('input["%s"] != "" && input["%s"] != null && input["%s"] != null && input["%s"].length > 0 && input["%s"] != null && input["%s"].length > 0', 
                        ns("custom_col_2"), ns("custom_col_2"), ns("custom_val_1"), ns("custom_val_1"), ns("custom_val_2"), ns("custom_val_2")),
      
      selectizeInput(
        ns("custom_col_3"),
        label = "Filter 3 - Column:",
        choices = NULL,
        multiple = FALSE
      ),
      
      conditionalPanel(
        condition = sprintf('input["%s"] != "" && input["%s"] != null', ns("custom_col_3"), ns("custom_col_3")),
        uiOutput(ns("custom_val_3_ui"))
      )
    ),

    # A second Apply button, so users who have scrolled down find one. It
    # needs its own id: buttons sharing an id drop clicks (app_server
    # observes both).
    actionButton(
      ns("apply_filters_btn_bottom"),
      "Apply Filters",
      class = "btn-primary w-100 mb-0",
      icon = icon("filter")
    ),

    actionButton(
      ns("clear_filters"),
      "Clear Filters",
      class = "btn-warning w-100"
    ),

    uiOutput(ns("download_ui"))
  )
}

#' Filters Server Module
#'
#' @param id Shiny module id
#' @noRd
mod_filters_server <- function(
  id,
  family_choices,
  genus_choices,
  taxon_name_choices,
  loading_from_url
) {
  moduleServer(
    id,
    function(input, output, session) {
      # React when taxon rank changes
      observeEvent(input$taxon_type, {
        if (loading_from_url()) return()
        
        # Clear family selection if switching away from family
        if (input$taxon_type != "family") {
          updateSelectizeInput(
            session,
            "family",
            choices = family_choices(),
            selected = NULL,
            server = TRUE
          )
        }

        if (input$taxon_type == "taxon_name") {
          updateSelectizeInput(
            session,
            "taxon_name",
            choices = taxon_name_choices(),
            selected = NULL,
            server = TRUE
          )

        } else if (input$taxon_type == "genus") {
          updateSelectizeInput(
            session,
            "genus",
            choices = genus_choices(),
            selected = NULL,
            server = TRUE
          )

        } else if (input$taxon_type == "family") {
          updateSelectizeInput(
            session,
            "family",
            choices = family_choices(),
            selected = NULL,
            server = TRUE
          )
        }
      })
      observeEvent(input$clear_filters, {

        updateRadioButtons(
          session,
          "taxon_type",
          selected = "all"
        )

        updateRadioButtons(
          session,
          "location",
          selected = character(0)
        )

        updateSelectizeInput(session, "family", selected = NULL)
        updateSelectizeInput(session, "genus", selected = NULL)
        updateSelectizeInput(session, "taxon_name", selected = NULL)
        updateSelectizeInput(session, "trait_name", selected = NULL)
        updateSelectizeInput(session, "trait_grouping", selected = NULL) 
        updateSelectizeInput(session, "structure_measured", selected = NULL) 
        updateSelectizeInput(session, "keywords", selected = NULL)
        updateSelectizeInput(session, "basis_of_record", selected = NULL)
        updateSelectizeInput(session, "life_stage", selected = NULL)
        updateSelectizeInput(session, "apc_taxon_distribution", selected = NULL)
        updateNumericInput(session, "min_latitude", value = NA)
        updateNumericInput(session, "max_latitude", value = NA)
        updateNumericInput(session, "min_longitude", value = NA)
        updateNumericInput(session, "max_longitude", value = NA)
        updateSelectizeInput(session, "custom_col_1", selected = character(0))
        updateSelectizeInput(session, "custom_val_1", selected = character(0))
        updateSelectizeInput(session, "custom_col_2", selected = character(0))
        updateSelectizeInput(session, "custom_val_2", selected = character(0))
        updateSelectizeInput(session, "custom_col_3", selected = character(0))
        updateSelectizeInput(session, "custom_val_3", selected = character(0))
      })

      # Dynamic UI for custom value inputs (dropdown vs text input)
      output$custom_val_1_ui <- renderUI({
        req(input$custom_col_1)
        
        if (input$custom_col_1 %in% controlled_vocab_columns) {
          selectizeInput(
            session$ns("custom_val_1"),  # USE session$ns
            label = "Filter 1 - Values:",
            choices = NULL,
            selected = character(0),
            multiple = TRUE
          )
        } else {
          textInput(
            session$ns("custom_val_1"),  # USE session$ns
            label = "Filter 1 - Search text:",
            placeholder = "Type to search..."
          )
        }
      })

      output$custom_val_2_ui <- renderUI({
        req(input$custom_col_2)
        
        if (input$custom_col_2 %in% controlled_vocab_columns) {
          selectizeInput(
            session$ns("custom_val_2"),  # USE session$ns
            label = "Filter 2 - Values:",
            selected = character(0),
            choices = NULL,
            multiple = TRUE
          )
        } else {
          textInput(
            session$ns("custom_val_2"),  # USE session$ns
            label = "Filter 2 - Search text:",
            placeholder = "Type to search..."
          )
        }
      })

      output$custom_val_3_ui <- renderUI({
        req(input$custom_col_3)
        
        if (input$custom_col_3 %in% controlled_vocab_columns) {
          selectizeInput(
            session$ns("custom_val_3"),  # USE session$ns
            label = "Filter 3 - Values:",
            selected = character(0),
            choices = NULL,
            multiple = TRUE
          )
        } else {
          textInput(
            session$ns("custom_val_3"),  # USE session$ns
            label = "Filter 3 - Search text:",
            placeholder = "Type to search..."
          )
        }
      })
    
      observeEvent(input$dataset_type, {
        if (input$dataset_type == "species") {
          updateRadioButtons(
            session,
            "location",
            choices = c(
              "None" = "",
              "APC taxon distribution" = "apc"
            ),
            selected = if (input$location == "georeferenced") "" else input$location
          )
        } else {
          updateRadioButtons(
            session,
            "location",
            choices = c(
              "None" = "",
              "Georeferenced records" = "georeferenced",
              "APC taxon distribution" = "apc"
            ),
            selected = input$location
          )
        }
      })

      # ---- DEBOUNCED INPUTS FOR BETTER PERFORMANCE ----
      # Debounce text inputs and numeric inputs to reduce reactive firing
      custom_val_1_debounced <- reactive(input$custom_val_1) |> debounce(800)
      custom_val_2_debounced <- reactive(input$custom_val_2) |> debounce(800)
      custom_val_3_debounced <- reactive(input$custom_val_3) |> debounce(800)
      
      min_latitude_debounced <- reactive(input$min_latitude) |> debounce(1000)
      max_latitude_debounced <- reactive(input$max_latitude) |> debounce(1000)
      min_longitude_debounced <- reactive(input$min_longitude) |> debounce(1000)
      max_longitude_debounced <- reactive(input$max_longitude) |> debounce(1000)
      
      # ---- RETURN FILTER STATE TO MAIN SERVER ----
      return(
        reactive({
          list(
            dataset_type = input$dataset_type,
            taxon_type = input$taxon_type,
            family = input$family,
            genus = input$genus,
            taxon_name = input$taxon_name,
            trait_filter_type = input$trait_filter_type,
            trait_name = input$trait_name,
            trait_grouping = input$trait_grouping,
            structure_measured = input$structure_measured,
            keywords = input$keywords,
            basis_of_record = input$basis_of_record,
            life_stage = input$life_stage,
            location = input$location,
            apc_taxon_distribution = input$apc_taxon_distribution,
            min_latitude = min_latitude_debounced(),
            max_latitude = max_latitude_debounced(),
            min_longitude = min_longitude_debounced(),
            max_longitude = max_longitude_debounced(),
            custom_col_1 = input$custom_col_1,
            custom_val_1 = custom_val_1_debounced(),
            custom_col_2 = input$custom_col_2,
            custom_val_2 = custom_val_2_debounced(),
            custom_col_3 = input$custom_col_3,
            custom_val_3 = custom_val_3_debounced()
          )
        })
      )
    }
  )
}
