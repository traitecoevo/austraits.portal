#' app_info UI Function
#'
#' @description A shiny Module for displaying app information
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_app_info_ui <- function(id){
  ns <- NS(id)
  
profile_links <- function(github = NULL, orcid = NULL) {
    tagList(
      if (!is.null(github)) tags$a(href = github, target = "_blank",
        tags$i(class = "fa-brands fa-github", style = "color: #24292e; font-size: 0.85em;"),
        title = "GitHub",
        style = "text-decoration: none;"),
      if (!is.null(orcid)) tags$a(href = orcid, target = "_blank",
        tags$i(class = "fa-brands fa-orcid", style = "color: #A6CE39; font-size: 0.85em;"),
        title = "ORCID",
        style = "margin-left: 5px; text-decoration: none;")
    )
  }
  
  # Reusable section header style
  hdr <- "color: #235c43; border-bottom: 2px solid #2f7d55; padding-bottom: 6px; margin-top: 7px; margin-bottom: 14px;"
  
  card(
    card_header("About and Information"),
    card_body(
      # ATTRIBUTION
      tags$h4(style = hdr, "Attribution"),
      
      p("The AusTraits Database Portal was designed by ",
        tags$strong("Pushkal Garg"), " ", profile_links(github = "https://github.com/Pushkalgithub", orcid = "https://orcid.org/0009-0002-0179-2946"), ", ",
        tags$strong("Fonti Kar"), " ", profile_links(github = "https://github.com/fontikar", orcid = "https://orcid.org/0000-0002-2760-3974"), ", ",
        tags$strong("Daniel Falster"), " ", profile_links(github = "https://github.com/dfalster", orcid = "https://orcid.org/0000-0002-9814-092X"), ", ",
        tags$strong("Elizabeth Wenk"), " ", profile_links(github = "https://github.com/ehwenk", orcid = "https://orcid.org/0000-0001-5640-5910"), ", and ",
        tags$strong("Ray Miles"), " ", profile_links(github = "https://github.com/raymiles"), "."
      ),
      
      p("Development of this portal was supported by a UNSW Research Infrastructure Grant. ",
        "AusTraits is a co-investment partnership with the Australian Research Data Commons (ARDC) ",
        "through the Planet Research Data Commons. ",
        "The ARDC is enabled by the Australian Government's National Collaborative Research Infrastructure Strategy (NCRIS)."),
      
      tags$style(".logo-link img { transition: transform 0.2s ease, opacity 0.2s ease; }
                  .logo-link img:hover { transform: scale(1.08); opacity: 0.75; cursor: pointer; }"),
      
      tags$div(
        style = "display: flex; gap: 24px; align-items: center; justify-content: center; padding: 24px 0; flex-wrap: wrap;",
        tags$a(href = "https://www.austraits.org", target = "_blank", class = "logo-link",
          tags$img(src = "https://austraits.org/images/austraits_hex.png", height = "70px", alt = "AUSTRAITS", style = "max-width: 140px;")),
        tags$a(href = "https://www.unsw.edu.au", target = "_blank", class = "logo-link",
          tags$img(src = "https://austraits.org/images/UNSW.png", height = "70px", alt = "UNSW")),
        tags$a(href = "https://www.westernsydney.edu.au/", target = "_blank", class = "logo-link",
          tags$img(src = "https://upload.wikimedia.org/wikipedia/en/f/f0/Western_Sydney_University_Crest.png", height = "70px", alt = "Western Sydney University", style = "max-width: 140px;")),
        tags$a(href = "https://www.botanicgardens.org.au", target = "_blank", class = "logo-link",
          tags$img(src = "https://nesplandscapes.edu.au/wp-content/uploads/2021/07/RBGS-Royal-Botanic-Gardens-Sydney.png", height = "70px", alt = "Royal Botanic Garden Sydney", style = "max-width: 140px;")),
        tags$a(href = "https://ardc.edu.au", target = "_blank", class = "logo-link",
          tags$img(src = "https://austraits.org/images/ARDC.png", height = "70px", alt = "ARDC")),
        tags$a(href = "https://www.education.gov.au/ncris", target = "_blank", class = "logo-link",
          tags$img(src = "https://bioplatforms.com/wp-content/uploads/2024/03/afb87787085b2b5c5a7814a28971e5aa-1.png", height = "70px", alt = "NCRIS", style = "max-width: 140px;"))
      ),
            
      tags$hr(),
      
      # BACKGROUND
      tags$h4(style = hdr, "Background"),
      
      p("The AusTraits Database Portal offers a web-based interface to filter, view and download subsets of the AusTraits Database. ",
        "The full database can also be downloaded from ",
        tags$a(href = "https://doi.org/10.5281/zenodo.3568417", target = "_blank", "Zenodo"),
        " as an .rds or .json file."),
      
      p("The Zenodo repository lists all ", tags$a(href = "https://doi.org/10.5281/zenodo.3568417", target = "_blank", "researchers"), " who have contributed their data to AusTraits. We thank them for being part of this project."),
      
      p("Additional AusTraits Project outputs to wrangle and interpret the data include:"),
      tags$ul(
        tags$li(tags$a(href = "https://github.com/traitecoevo/austraits", target = "_blank", "austraits"), " - an R package to explore and wrangle the AusTraits Database"),
        tags$li(tags$a(href = "https://github.com/traitecoevo/APD", target = "_blank", "AusTraits Plant Dictionary"),
          " \u2013 formal definitions of all traits included in the database")
      ),
      
      p("Please visit our ", tags$a(href = "https://www.austraits.org", target = "_blank", "website"), " for more project information."),
      
      p("The AusTraits database is facilitating research on Australia's diverse flora, including functional traits research, the preservation of Australia's unique plants, predicting species' responses to climate change, and understanding ecosystem dynamics at a continental scale."),
      
      tags$hr(),

      # HOW TO USE
      tags$h4(style = hdr, "How to Use"),
      
      tags$div(
        style = "background: #f2f6f3; border-left: 4px solid #2f7d55; padding: 10px 15px; border-radius: 4px; margin-bottom: 18px;",
        tags$strong("Quick start:"),
        " Select a dataset type \u2192 set your taxonomy \u2192 pick a trait (optional) \u2192 explore."
      ),
      
      p(tags$strong("Dataset type \u2014"),
        "Raw data or Species averages? Raw data has the individual observations submitted by contributors while Species averages presents species-level summary statistics (mean, min, max, median for numeric traits and value summaries for categorical traits). ",
        "Pick based on the question you're asking."),
      
      p(tags$strong("Taxonomy \u2014"),
        "Choose a taxonomic filter - Select All taxa or Family, Genus, or Taxon name (species and infraspecies), then select from the dropdown that appears. ",
        "Family (Fabaceae) is selected as the default starting point."),
      
      p(tags$strong("Traits \u2014"),
        "Search or browse the trait dropdown \u2013 noting you can select multiple traits at a time. ",
        "Switch to ", tags$strong("Trait features"), " if you want to search by trait groupings, measured structures, or keywords. ",
        "Enabling trait features filters also refines the trait selection in the dropdown menu, for easier searching. ",
        "Note, Trait features is only enabled if a trait name is not yet selected."),
      
      p(tags$strong("Location \u2014"),
        "Data can be filtered by taxon distribution (per the APC) or by observation coordinates. You can opt to display all georeferenced data or specify a bounding box, filtering to data collected within a specific region of Australia. ",
        "The ", tags$strong("APC taxon distribution"), " (state/territory) filter works for both raw data and species average data outputs, while the ",
        tags$strong("Georeferenced records"), " option is only enabled when Raw data are displayed."),
      
      p(tags$strong("Custom filters \u2014"),
        "Add up to three additional column filters to fine-tune the data displayed. ",
        "An additional filter slot appears once you've filled in a column name and values for the first filter. Columns with a controlled vocabulary (e.g. ", tags$strong("life_stage"), ") have a drop-down menu of allowed options, while other columns (e.g. ", tags$strong("context property"), ") accept any text. Please use ", tags$strong("|"), " between two free-text filter queries to multi-select."),
      
      p(tags$strong("Data Preview \u2014"),
        "Your filtered results, viewable 10, 25, 50, or 100 rows at a time. Columns are sortable."),
      
      tags$hr(),
      
      # USAGE METRICS
      tags$h4(style = hdr, "Portal Usage"),

      # Metric cards row
      tags$div(
        style = "display: flex; gap: 16px; flex-wrap: wrap; margin-bottom: 20px;",
        
        # Sessions card
        tags$div(
          style = "flex: 1; min-width: 140px; background: #f2f6f3; border-radius: 10px; padding: 18px; text-align: center;",
          tags$div(style = "font-size: 1.8em; font-weight: 700; color: #235c43;", uiOutput(ns("metric_sessions"))),
          tags$div(style = "font-size: 0.82em; color: #64726b; margin-top: 4px;",
            icon("users", style = "color: #2f7d55;"), " Total Sessions")
        ),

        # Searches card
        tags$div(
          style = "flex: 1; min-width: 140px; background: #f3f7f8; border-radius: 10px; padding: 18px; text-align: center;",
          tags$div(style = "font-size: 1.8em; font-weight: 700; color: #2a7f9e;", uiOutput(ns("metric_searches"))),
          tags$div(style = "font-size: 0.82em; color: #64726b; margin-top: 4px;",
            icon("search", style = "color: #2a7f9e;"), " Searches")
        ),

        # Downloads card
        tags$div(
          style = "flex: 1; min-width: 140px; background: rgba(217, 164, 65, 0.14); border-radius: 10px; padding: 18px; text-align: center;",
          tags$div(style = "font-size: 1.8em; font-weight: 700; color: #9a6f16;", uiOutput(ns("metric_downloads"))),
          tags$div(style = "font-size: 0.82em; color: #64726b; margin-top: 4px;",
            icon("download", style = "color: #d9a441;"), " Downloads")
        )
      ),

      tags$p(style = "font-size: 0.8em; color: #90a4ae;", 
        "Metrics are updated in real time and reflect cumulative portal activity."),    

tags$div(
  style = "display: flex; flex-direction: column;",
  tags$div(
    style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
    tags$h4(style = hdr, "Monthly Breakdown"),
    tags$div(
      downloadButton(ns("download_csv"), "Download CSV", style = "margin-right: 10px;"),
      downloadButton(ns("download_json"), "Download JSON")
    )
  ),
  DT::DTOutput(ns("monthly_data_table"))
),
      # USAGE GUIDELINES
      tags$h4(style = hdr, "Usage Guidelines"),
      
      p(tags$strong("Citing AusTraits \u2014"),
        "The ", tags$strong("Citations"), " tab indicates the proper attribution for your selected AusTraits data."),
      
      p(tags$strong("Feedback \u2014"),
        "Something off? A suggestion bubbling up? We're always keen to hear from people using the portal. ",
        "Get in touch through the ", tags$a(href = "https://www.austraits.org", target = "_blank", "AusTraits website"), ".")
    )
  )
}

#' app_info Server Functions
#'
#' @noRd 
mod_app_info_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

# Refreshing using Supabase REST API
	metrics <- reactive({
  # Auto-refresh every 60 seconds
  invalidateLater(60000, session)
  
  tryCatch({
    # Read all telemetry data from Supabase
    data <- read_telemetry_monthly_cached()
    
    if (nrow(data) > 0) {
      # Count by event type
      list(
        sessions = sum(data$n[data$type == "login"], na.rm = TRUE),
        searches = sum(data$n[data$type == "search"], na.rm = TRUE),
        downloads = sum(data$n[data$type == "download"], na.rm = TRUE)
      )
    } else {
      list(sessions = 0, searches = 0, downloads = 0)
    }
  }, error = function(e) {
       list(sessions = 0, searches = 0, downloads = 0)
  })
})

# Total sessions
output$metric_sessions <- renderUI({
  m <- metrics()
  tags$span(format(m$sessions, big.mark = ","))
})

# Total searches
output$metric_searches <- renderUI({
  m <- metrics()
  tags$span(format(m$searches, big.mark = ","))
})

# Total downloads
output$metric_downloads <- renderUI({
  m <- metrics()
  tags$span(format(m$downloads, big.mark = ","))
})
# Monthly metrics table
output$monthly_data_table <- DT::renderDT({
  tryCatch({
    monthly_data <- get_monthly_metrics("2020-01-01", as.character(Sys.Date() + 1))
    
    dt <- DT::datatable(
      monthly_data,
      escape = FALSE,
      rownames = FALSE,
      filter = "none",
      colnames = c("Month", "Logins", "Searches", "Downloads"),
      options = list(pageLength = 12, searching = FALSE, ordering = FALSE)
    )
    dt
  }, error = function(e) {
    DT::datatable(data.frame(Error = e$message))
  })
})
# Download CSV
output$download_csv <- downloadHandler(
  filename = function() {
    paste0("austraits_counter_metrics_", Sys.Date(), ".csv")
  },
  content = function(file) {
    df <- export_counter_csv("2020-01-01", as.character(Sys.Date() + 1))
    write.csv(df, file, row.names = FALSE)
  }
)

# Download JSON
output$download_json <- downloadHandler(
  filename = function() {
    paste0("austraits_counter_metrics_", Sys.Date(), ".json")
  },
  content = function(file) {
    json_data <- export_counter_json("2020-01-01", as.character(Sys.Date() + 1))
    writeLines(json_data, file)
  }
)
  })
}
