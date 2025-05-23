#' AusTraits Data Portal Server Logic
#'
#' @param input Input list generated by Shiny
#' @param output Output list generated by Shiny
#' @param session Session id for Shiny interaction

austraits_server <- function(input, output, session) {
  # Reactive value to store the filtered data later
  filtered_database <- reactiveVal(NULL)
  
  # New reactive value to store datatable filter state
  dt_proxy <- reactiveVal(NULL)

  # Contains the data usage text 
  usage_text <- reactiveVal(NULL)
  
  # Contains the taxon text
  taxon_text <- reactiveVal(NULL)

  # Initialize dropdown choices
  taxon_name_choices <- reactive({
    all_taxon_names
  })
  genus_choices <- reactive({
    all_genus
  })
  family_choices <- reactive({
    all_family
  })
  
  # Update the appropriate selectizeInput when radio button changes
  observeEvent(input$taxon_rank, {
    # Reset the filtered database to clear the data preview
    filtered_database(NULL)
    
    # First, clear the Fabaceae selection from family if switching to another rank
    if (input$taxon_rank != "family") {
      updateSelectizeInput(
        session,
        "family",
        choices = family_choices(),
        selected = NULL,
        server = TRUE
      )
    }
    # Then update the appropriate input based on selected rank
    if (input$taxon_rank == "taxon_name") {
      updateSelectizeInput(
        session,
        "taxon_name",
        choices = taxon_name_choices(),
        selected = NULL,
        server = TRUE
      )
    } else if (input$taxon_rank == "genus") {
      updateSelectizeInput(
        session,
        "genus",
        choices = genus_choices(),
        selected = NULL,
        server = TRUE
      )
    } else if (input$taxon_rank == "family") {
      updateSelectizeInput(
        session,
        "family",
        choices = family_choices(),
        selected = "Fabaceae",
        server = TRUE
      )
    }
  })
  
  # Server-side selectizeInput update for other options that are not conditional
  updateSelectizeInput(session, "trait_name", choices = all_traits, server = TRUE)
  updateSelectizeInput(session, "basis_of_record", choices = all_bor, server = TRUE)
  updateSelectizeInput(session, "life_stage", choices = all_age, server = TRUE)
  
  # Apply Filter
  observeEvent(list(
    input$family,
    input$genus,
    input$taxon_name,
    input$trait_name,
    input$basis_of_record,
    input$life_stage,
    input$location,
    input$apc_taxon_distribution
  ), {
    # At start up, we want filters set to false
    valid_filters <- valid_filters(input)
    
    # Check if any filter has values using our helper function
    has_filters <- any(sapply(valid_filters, function(name) {
      has_input_value(input, name)
    }))
    
    if (has_filters) {
      # Convert input to a regular list first
      input_values <- reactiveValuesToList(input)

      # Apply filters with the input values
      filtered_data <- austraits |>
        apply_filters_categorical(input_values) |>
        apply_filters_location(input_values) |> 
        dplyr::collect()

      usage_text(generate_usage_and_citations_text(filtered_data))
      output$usage_text <- renderUI({usage_text()})

      trait_profile <- generate_trait_profile(filtered_data)

      # TODO: for some reason the leaflet plot is not rendering
      output$trait_profile <- renderUI({
        tagList(
          trait_profile[[1]]
          # HTML("<b>Trait histogram:</b>"),
          # trait_profile[[2]],
          # HTML("<b>Trait table:</b>"),
          # trait_profile[[3]],
          # HTML("<b>Trait map:</b>"),
          # leaflet::renderLeaflet(trait_profile[[4]])
        )
      })

      output$trait_histogram_text <- renderUI({
        tagList(
          p("The plot below shows the distribution of selected data for this trait, including data collected on individuals of all age classes (seedling, sapling, adult), both field-collected and experimental data, and data representing individuals and population means."),
          p("Visualising data records across the families with the most data for the trait indicates the taxonomic breadth of information for this trait"),
          # em("Trait histogram:") #TODO: Not sure if this is needed
        )
      })

      output$trait_beeswarm_plot <- plotly::renderPlotly({
        req(filtered_data, input$trait_name)
        plot_trait_distribution(filtered_data, input$trait_name) |>
          plotly::ggplotly(tooltip = c("x", "y", "text"), height = 400)
      })

      output$trait_geo_text <- renderUI({
          trait_profile[[3]]
        
      })

      output$trait_geo_map <- leaflet::renderLeaflet({
        trait_profile[[4]]
      })


      # Store filtered data into reactive value
      filtered_database(filtered_data)
    } else {
      # No filters selected
      filtered_database(NULL)

      output$trait_profile <- renderUI({
        tagList(
          HTML("Please select a single trait to view.")
        )
      })
    }
  })
    
# Display all data when all taxa are selected
  observeEvent(
    input$taxon_rank, {
    if(input$taxon_rank == "all") {
      # If not taxonomic rank is selected, show full database
      full_database <- austraits |> dplyr::collect()
      filtered_database(full_database)
    } 
    else {
      if (is.null(filtered_database())) {
        return()
      }
    }
  }) 

  # Check the taxon_name selection (input$taxon_name) when switching to "Taxon View" tab
  observeEvent(list(
                    input$main_tabs,
                    input$taxon_rank,
                    input$taxon_name
  ), {
    # First, handle cases where we need to clear the taxon view
      # Case 1: Taxon rank is not "taxon_name"
      # Case 2: taxon_name is NULL or empty
      if (input$taxon_rank != "taxon_name" || is.null(input$taxon_name) || length(input$taxon_name) == 0) {
        # Clear the taxon text
        taxon_text(NULL)
        output$taxon_text <- renderUI({NULL})
      }
    
    # Check if the current tab is "Taxon View"
    if (input$main_tabs == "Taxon View") {
      # browser()
      # Check if taxon_rank is "taxon_name"
      if (!input$taxon_rank == "taxon_name" || is.null(input$taxon_name)) {
          showNotification(
            "Only a single taxon name can be used for Taxon View",
            type = "warning",
            duration = 5
          )
        }
        
        # Check if taxon_name is NULL (nothing selected)
      else if (is.null(input$taxon_name) || length(input$taxon_name) == 0) {
          showNotification(
            "Please select a single taxon name for Taxon View",
            type = "warning",
            duration = 5
          )
        }
        # Check if multiple taxa are selected
      else if (length(input$taxon_name) > 1) {
          showNotification(
            "Please select a single taxon name for Taxon View",
            type = "warning",
            duration = 5
          )
        }

  # Generate Taxon View text if passes all checks
      else if (input$taxon_rank == "taxon_name" && !is.null(input$taxon_name) && length(input$taxon_name) == 1) {
        
        # Get the filtered data    
        data <- filtered_database()
        
        # Check if data is NULL or if user has manually cleared filters
        if (is.null(data)) {
          # Apply a filter just for this taxon to generate the taxon view
          data <- austraits |>
            apply_filters_categorical(input) |>
            dplyr::collect()
          
          # Update the filtered_database reactive
          filtered_database(data)
        }
        
        # Now we can use the data (whether it was already filtered or we just created it)
        if (nrow(data) > 0) {
          # Generate the taxon text
          taxon_text(generate_taxon_text(data, input$taxon_name))
          output$taxon_text <- renderUI({HTML(commonmark::markdown_html(taxon_text()))})
        } else {
          # If no data is available, show a notification
          showNotification(
            "No data available for the selected taxon name",
            type = "warning",
            duration = 5
          )
          # Clear the taxon text
          taxon_text(NULL)
          output$taxon_text <- renderUI({NULL})
        }
      }
    }
  }, ignoreInit = TRUE)

  # Clear filters button action
  observeEvent(input$clear_filters, {
    
  # Check if any filters are currently applied
  filters_applied <- !is.null(input$taxon_rank) || 
                     !is.null(input$trait_name) || 
                     !is.null(input$basis_of_record) || 
                     !is.null(input$life_stage) || 
                     !is.null(input$apc_taxon_distribution) || 
                     !is.null(input$location)
  
  if (!filters_applied) {
    # Show a notification if no filters are applied
    showNotification("No filters are currently applied",
                     type = "warning",
                     duration = 3)
    return() # Exit the observer early
  }

    # Based on which filter is currently active
    if (input$taxon_rank == "taxon_name") {
      updateSelectizeInput(
        session,
        "taxon_name",
        choices = taxon_name_choices(),
        selected = NULL,
        server = TRUE
      )
    } else if (input$taxon_rank == "genus") {
      updateSelectizeInput(
        session,
        "genus",
        choices = genus_choices(),
        selected = NULL,
        server = TRUE
      )
    } else if (input$taxon_rank == "family") {
      updateSelectizeInput(
        session,
        "family",
        choices = family_choices(),
        selected = NULL,
        server = TRUE
      )
    }
    
    # Clear the other filters that are not conditional
    updateSelectizeInput(session, "trait_name", choices = all_traits, server = TRUE)
    updateSelectizeInput(session, "basis_of_record", choices = all_bor, server = TRUE)
    updateSelectizeInput(session, "life_stage", choices = all_age, server = TRUE)
    updateSelectizeInput(session, "apc_taxon_distribution", choices = all_states_territories, server = TRUE)
    
    # Clear the radio button selection
    updateRadioButtons(session, "location", selected = character(0))
    updateRadioButtons(session, "taxon_rank", selected = character(0))

    # Set the filtered database to NULL
    filtered_database(NULL)

    # Reset the download data to NULL
    download_data_table <- reactive({
      NULL
    })
 
    # Reset datatable filters
    if (!is.null(dt_proxy())) {
      DT::replaceData(dt_proxy(), display_data_table())
    }

    # Reset the usage text
    usage_text(NULL)
    
    # Show notification
    showNotification("Filters have been cleared",
                     type = "message",
                     duration = 3
    )
  })
  
  # Set up display data as reactive expression
  display_data_table <- reactive({
    # Get the current filtered database
    filtered_db <- filtered_database()
    
    # Check if it's NULL and return appropriate value
    if (is.null(filtered_db)) {
      return(NULL)
    }
    
    # Format the database for display
    format_database_for_display(filtered_db) |> 
      format_hyperlinks_for_display()
  })
  
  # Set up download data as reactive expression
  download_data_table <- reactive({
    # Get the current filtered database
    filtered_db <- filtered_database()
    display_db <- display_data_table()
    
    # Check if it's NULL and return appropriate value
    if (is.null(filtered_db)) {
      return(NULL)
    }
    
    # Get the row indices from the DT table that are currently visible
    # after filtering in the datatable
    if (!is.null(input$data_table_rows_all)) {
      # Get all visible row indices after filtering
      visible_rows <- input$data_table_rows_all
      
      # Subset the display data with the visible row indices
      display_db_filtered <- display_db[visible_rows, , drop = FALSE]
      
      # Join back up to full dataset to get all columns for only the visible rows
      return(filtered_db |> dplyr::semi_join(display_db_filtered, by = "row_id"))
    }
    
    # Default: Join back up to full dataset to get all columns
    filtered_db |> dplyr::semi_join(display_db, by = "row_id")
  })
  
  # Render user selected data table output
  output$data_table <- DT::renderDT({
    # Get the display data
    display_data <- display_data_table()
    
    # Return NULL or empty table if no data
    if (is.null(display_data)) {
      return(datatable(data.frame(), options = list(pageLength = 10)))
    }
    
    # Truncate text columns to 20 characters except for column names listed below
    display_data_truncated <- display_data
    text_columns <- sapply(display_data, is.character)
    columns_to_exclude <- c("taxon_name", "trait_name", "genus", "family", "source_primary_citation")  # Exclude the hyperlink column from truncation
    
    for (col in names(display_data)[text_columns]) {
      # Skip the excluded columns
      if (col %in% columns_to_exclude) next
      
      display_data_truncated[[col]] <- sapply(display_data[[col]], function(x) {
        if (is.na(x) || is.null(x)) return(x)
        if (nchar(x) > 20) {
          paste0(substr(x, 1, 20), "...")
        } else {
          x
        }
      })
    }
    # Determine column indices where we want to turn off column filtering
    no_filter_cols <- which(names(display_data_truncated) %in% c("value", "unit", "entity_type", "value_type", "replicates"))
    # Hide the row_id column
    hide_cols <- which(names(display_data_truncated) %in% c("row_id"))
    dt <- datatable(
      data = display_data_truncated,
      escape = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = FALSE,
        columnDefs = list(
          list(
            searchable = FALSE, 
            targets = no_filter_cols - 1 # Targets denotes the columns index where filter will be switched off - Note that JS is 0 indexing
          ),
          list(
            visible = FALSE,
            targets = hide_cols - 1 # hide these columns from table view
          )
        ),
        # Add server-side processing for better filtering performance
        serverSide = FALSE # Keep this as FALSE for client-side filtering to access filtered rows
      ),
      rownames = FALSE,
      filter = "top",
      class = "cell-border stripe nowrap",
      # Removed selection = 'multiple' option
    )
    
    # Store the DT proxy for later use
    dt_proxy(DT::dataTableProxy("data_table"))
    
    return(dt)
  })
  
  # Update info message about visible rows
  output$rows_info <- renderUI({
    # Get the number of rows currently displayed after filtering
    if (!is.null(input$data_table_rows_all)) {
      total_visible <- length(input$data_table_rows_all)
      return(HTML(paste0("<span> Download will include ", total_visible, " observations.</span>")))
    }
    return(NULL)
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("austraits-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Create a temporary directory that will hold the zip file contents
      # A new temp directory is made since the one returned by tempdir() is in use
      # by other libraries
      tmpdir = tempfile(pattern="tempdir", fileext = ".dir")
      dir.create(tmpdir)
      csv_file <- file.path(tmpdir, "austraits-data.csv")
      bib_file <- file.path(tmpdir, "sources.bib")
      html_file <- file.path(tmpdir, "usage.html")

      # Get the current download data with datatable filtering applied
      data_to_download <- download_data_table()

      # Handle NULL or empty data case
      if (is.null(data_to_download) || nrow(data_to_download) == 0) {
        data_to_download <- data.frame(message = "No data selected")
      }

      # Download austraits data
      utils::write.csv(data_to_download, csv_file, row.names = FALSE)

      # Export bibtex from the filtered data
      keys <- data_to_download$source_primary_key |> unique()
      export_bibtex_for_data(keys, filename = bib_file)

      # Update the usage text and convert to html
      usage_text(generate_usage_and_citations_text(data_to_download))
      htmltools::save_html(usage_text(), html_file)

      # Show notification
      showNotification("Downloading filtered data...",
                       type = "message",
                       duration = 3)


      zip::zip(
        zipfile = file, 
        files = list.files(tmpdir, full.names = TRUE), 
        mode = "cherry-pick"
      )

      # Clean up temporary directory
      unlink(tmpdir, recursive = TRUE)
    },
    contentType = "application/zip"
  )
}
