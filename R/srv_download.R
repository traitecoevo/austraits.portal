#' Server logic for data download
#'
#' @description Handles download UI and download handler
#' @param input,output,session Shiny session objects
#' @param filtered_query_cache reactiveVal holding the current lazy query
#' @param filtered_total reactiveVal holding the query's row count
#' @param current_austraits Reactive for current full dataset
#' @param filters Reactive filters from mod_filters_server
#'
#' @noRd
srv_download <- function(input, output, session,
                         filtered_query_cache,
                         filtered_total,
                         current_austraits,
                         filters) {

  # DOWNLOAD DATA PREPARATION

  # The full-column rows matching the current filters, as a lazy query. The
  # join runs in DuckDB; collecting the whole table into R first took ~10 s
  # and over 1 GB per download.
  download_query <- reactive({
    query <- filtered_query_cache()
    if (is.null(query)) {
      return(NULL)
    }
    current_austraits() |>
      dplyr::semi_join(query |> dplyr::select(row_id), by = "row_id")
  })

  # DYNAMIC DOWNLOAD UI WITH WARNING

  output[["filters-download_ui"]] <- renderUI({
    total_rows <- filtered_total()

    if (is.null(total_rows)) {
      return(downloadButton("filters-download_data", "Download Data", class = "btn-primary w-100", icon = icon("download")))
    }

    if (total_rows >= 100000) {
      # Large dataset - show warning
      tagList(
        div(
          style = "background: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 10px; margin-bottom: 10px;",
          icon("exclamation-triangle", style = "color: #856404;"),
          span(
            style = "color: #856404; margin-left: 5px; font-weight: bold;",
            sprintf("⚠️ Large download: %s rows", format(total_rows, big.mark = ","))
          )
        ),
        downloadButton(
          "filters-download_data",
          sprintf("Confirm & Download (%s rows)", format(total_rows, big.mark = ",")),
          class = "btn-warning w-100",
          icon = icon("download")
        )
      )
    } else {
      # Small dataset - direct download
      downloadButton("filters-download_data", "Download Data", class = "btn-primary w-100", icon = icon("download"))
    }
  })

  # DOWNLOAD HANDLER

  output[["filters-download_data"]] <- downloadHandler(
    filename = function() {
      paste("austraits-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      tmpdir = tempfile(pattern="tempdir", fileext = ".dir")
      dir.create(tmpdir)
      on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
      csv_file <- file.path(tmpdir, "austraits-data.csv")
      bib_file <- file.path(tmpdir, "sources.bib")
      sources_file <- file.path(tmpdir, "sources.csv")
      html_file <- file.path(tmpdir, "usage.html")

      data_query <- download_query()

      if (is.null(data_query)) {
        data.frame(message = "No data selected") |>
          utils::write.csv(csv_file, row.names = FALSE)
        writeLines("", bib_file)
        writeLines("<p>No data selected</p>", html_file)
      } else {
        write_query_csv(data_query, csv_file)

        # Only collect distinct keys, not the data
        key_rows <- data_query |>
          dplyr::distinct(source_primary_key) |>
          dplyr::collect()
        keys <- key_rows$source_primary_key |>
          # This handles multiple keys pasted together with "; ", an issue for species averages
          stringr::str_split(pattern = "; ") |> unlist() |> sort() |> unique()

        sources |>
          dplyr::filter(source_primary_key %in% keys) |>
          arrow::write_csv_arrow(sources_file)

        export_bibtex_for_data(keys, bib_file)

        usage_text_content <- generate_usage_and_citations_text(key_rows)
        htmltools::save_html(usage_text_content, html_file)
      }

      # Log download event
      log_telemetry_event("download", details = list(
        dataset_type = filters()$dataset_type,
        trait = filters()$trait_name
      ))

      showNotification("Downloading filtered data...",
                      type = "message",
                      duration = 3)

      zip::zip(
        zipfile = file,
        files = list.files(tmpdir, full.names = TRUE),
        mode = "cherry-pick"
      )
    },
    contentType = "application/zip"
  )
}

#' Write a lazy DuckDB query to CSV without loading it into R
#'
#' @param query Lazy DuckDB query.
#' @param path Output CSV path.
#' @noRd
write_query_csv <- function(query, path) {
  con <- dbplyr::remote_con(query)
  sql <- dbplyr::sql_render(query, con = con)
  DBI::dbExecute(con, sprintf("COPY (%s) TO %s (HEADER, DELIMITER ',')", sql, DBI::dbQuoteString(con, path)))
  invisible(path)
}
