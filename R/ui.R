#' User interface (UI) for AusTraits Data Portal

austraits_ui <- function() {
  ui <- page_sidebar(

    # Set the overall theme of the app
    theme = bs_theme(preset = "flatly"),

    # Title of the portal
    title = "AusTraits Data Portal",

    footer =  tags$footer(
      "Powered by ",
      tags$a(href = "https://www.unsw.edu.au/science", "UNSW Faculty of Science"), 
      align = "right", style = "padding: 30px",
      
      div("Created by AusTraits Team",  
          target)
      ),

    # Create a sidebar for the app
    sidebar = sidebar(
      # Filter by taxonomic information
      h5("Taxonomy"),
      radioButtons("taxon_rank",
        label = "Filter by which taxon rank:",
        choices = c(
          "All taxa" = "all",
          "Family" = "family",
          "Genus" = "genus",
          "Taxon name" = "taxon_name"
        ),
        selected = "family" 
      ),

      # Only show this panel if Taxon name is selected
      conditionalPanel(
        condition = 'input.taxon_rank == "taxon_name"',
        ## By taxon_name
        selectizeInput("taxon_name",
          label = "Taxon name:",
          choices = NULL,
          multiple = TRUE
        )
      ),

      # Only show this panel if Genus is selected
      conditionalPanel(
        condition = 'input.taxon_rank == "genus"',
        ## By genus
        selectizeInput("genus",
          label = "Genus:",
          choices = NULL,
          multiple = TRUE
        )
      ),
      # Only show this panel if family is selected
      conditionalPanel(
        condition = 'input.taxon_rank == "family"',
        ## By family
        selectizeInput("family",
          label = "Family:",
          choices = NULL,
          multiple = TRUE, 
          selected = "Fabaceae",
        )
      ),
      # Filter by trait information
      h5("Trait"),
      ## By trait name
      selectizeInput("trait_name",
        label = "Trait name(s):",
        choices = NULL,
        multiple = TRUE,
        options = list(
          create = TRUE
        )
      ),

      # Filter by location information
      h5("Location"),
      radioButtons("location",
        label = "Filter by which location filter:",
        choices = c(
          "Georeferenced records" = "georeferenced",
          # "Enter coordinates" = "enter_coordinates",
          # "Recorded state/territory" = "state",
          "APC taxon distribution" = "apc"
        ),
        selected = character(0) # No default selection
      ),
      # User chooses to input coordinates
      conditionalPanel(
        condition = 'input.location == "enter_coordinates"',
        ## Input coordinates
        textInput("coordinates",
          label = "Coordinates:",
          # value = "-33.92, 151.24", # UNSW Kensington from Google Maps
          placeholder = "-33.92, 151.24"
        )
      ),

      # State in APC
      conditionalPanel(
        condition = 'input.location == "apc"',
        ## By State by APC
        selectizeInput("apc_taxon_distribution",
          label = "State/territory:",
          choices = all_states_territories,
          multiple = TRUE
        )
      ),

      # State by state in location property
      conditionalPanel(
        condition = 'input.location == "state"',
        ## By State in Location Property
        selectizeInput("state",
          label = "State/territory:",
          choices = NULL,
          multiple = TRUE
        )
      ),
      h5("Additional"),
      ## By BoR
      # TODO: Hard code some options e.g field to select all field values
      selectizeInput("basis_of_record",
        label = "Basis of Record:",
        choices = NULL,
        multiple = TRUE
      ),

      ## By lifestage
      # TODO: Hard code some options e.g saplings to select all saplings
      selectizeInput("life_stage",
        label = "Life stage:",
        choices = NULL,
        multiple = TRUE
      ),
      br(),
      actionButton("clear_filters", "Clear Filters",
        class = "btn-warning w-100"
      ),

      # Download button
      uiOutput("rows_info"),
      downloadButton("download_data", "Download displayed data")
    ),

    # Data display
    navset_bar(
      id = "main_tabs",

      nav_panel(
        title = "Data Preview",
        card(
          card_body(
            fillable = TRUE,
            DT::DTOutput("data_table")
          )
        )
      ),
      nav_panel(
        title = "App Information",
        card(
          card_header("How to Use the App"),
          card_body(
            fillable = TRUE,
            p("This application allows users to filter and explore the AusTraits dataset."),
            p("Use the sidebar to apply filters based on taxonomy, traits, location, and additional criteria."),
            p("Filtered data will be displayed in the 'Data Preview' tab."),
            p("You can download the filtered data using the 'Download displayed data' button."),
            tags$a(href = "https://www.austraits.org.au", "AusTraits Website")
          )
        )
      ),
      nav_panel(
        title = "Taxon View",
        card(
          card_header("Information about selected Taxon"),
          card_body(
            fillable = TRUE,
            p("This panel displays information about the selected taxon."),
            p("You can view the taxon name, family, and other relevant details."),
            p("Use the sidebar to filter the data based on different criteria."),
          )
        )
      ),
      nav_panel(
        title = "Trait View",
        card(
          card_header("Information about selected trait"),
          card_body(
            fillable = TRUE,
            p("This panel displays information about the selected trait."),
            p("You can explore the trait data and its distribution across different taxa."),
            p("Use the sidebar to filter the data based on different criteria."),
          )
        )
      )
    )
  )
}
