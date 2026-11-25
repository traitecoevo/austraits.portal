#' data_table UI Function
#'
#' @description A shiny Module for displaying the data table
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_table_ui <- function(id){
  ns <- NS(id)

  tagList(
    # Add custom CSS for sticky header
    tags$style(HTML("
      .dataTables_scrollHead {
        position: sticky !important;
        top: 0 !important;
        z-index: 100 !important;
        background: white !important;
      }
      .dataTables_scrollHeadInner table thead {
        background: white !important;
      }
    ")),

    card(
      card_body(
        div(
          DT::DTOutput(ns("data_table"))
        )
      )
    )
  )
}

#' Columns shown in the data table, in display order
#'
#' Family, genus, taxon name and trait name first, then the dataset's display
#' columns that exist in the query.
#' @noRd
table_display_columns <- function(available, columns_display) {
  priority_cols <- c("family", "genus", "taxon_name", "trait_name")
  cols <- c(priority_cols, setdiff(columns_display, priority_cols))
  cols[cols %in% available]
}

#' The query the data table pages through
#'
#' Selects the display columns (plus `row_id`, used as a sort tie-breaker so
#' pages never overlap). For species means, `value` shows the observed mean
#' with its unit, or for a categorical trait the most frequent value
#' (`value_mode`), so one column reads for both.
#' @noRd
table_query <- function(query, cols) {
  available <- colnames(query)
  if (is_species_means(available)) {
    query <- query |>
      dplyr::mutate(
        value = dplyr::case_when(
          type == "categorical" ~ value_mode,
          !is.na(unit) & unit != "" ~ paste0(as.character(value), " ", unit),
          TRUE ~ as.character(value)
        )
      )
  }
  keep <- c(cols, intersect("row_id", setdiff(available, cols)))
  query |> dplyr::select(dplyr::all_of(keep))
}

#' Is this table the published species means?
#' @param columns Column names.
#' @noRd
is_species_means <- function(columns) {
  all(c("value", "value_mode", "unit", "type") %in% columns)
}

#' Fetch one page of a lazy query from DuckDB
#'
#' @param query Lazy query from table_query().
#' @param cols Columns to return.
#' @param start Zero-based offset of the first row.
#' @param length Number of rows; -1 for all.
#' @param order_col Column to sort by, or NULL for the stored order.
#' @param descending Sort descending?
#' @return data.frame with `cols`.
#' @noRd
fetch_table_page <- function(query, cols, start, length, order_col = NULL, descending = FALSE) {
  con <- dbplyr::remote_con(query)
  inner <- dbplyr::sql_render(query, con = con)
  order_sql <- ""
  if (!is.null(order_col)) {
    direction <- if (descending) "DESC" else "ASC"
    col_sql <- DBI::dbQuoteIdentifier(con, order_col)
    keys <- sprintf("%s %s NULLS LAST", col_sql, direction)
    # These columns hold numbers stored as text; sort numbers numerically
    # ("99" before "1000"), then any non-numeric values as text
    if (order_col %in% c("value", "replicates")) {
      # (species-mean values carry a unit suffix, so take the leading number)
      keys <- c(sprintf("TRY_CAST(regexp_extract(CAST(%s AS VARCHAR), '^-?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?') AS DOUBLE) %s NULLS LAST",
                        col_sql, direction), keys)
    }
    if ("row_id" %in% colnames(query) && order_col != "row_id") keys <- c(keys, "row_id")
    order_sql <- paste(" ORDER BY", paste(keys, collapse = ", "))
  }
  limit_sql <- if (length >= 0) sprintf(" LIMIT %d OFFSET %d", as.integer(length), as.integer(start)) else ""
  sql <- sprintf("SELECT * FROM (%s) AS table_page%s%s", inner, order_sql, limit_sql)
  page <- DBI::dbGetQuery(con, sql)
  page[, cols, drop = FALSE]
}

#' DataTables server-side handler backed by DuckDB
#'
#' DT calls this for every page, sort and length change. It reads the current
#' query from `state`, so a new result never needs the handler replaced.
#' @param state Environment with `query`, `cols` and `total`.
#' @noRd
duckdb_table_filter <- function(state) {
  function(data, params) {
    draw <- as.integer(params$draw)
    if (is.null(state$query)) {
      return(list(draw = draw, recordsTotal = 0, recordsFiltered = 0, data = list(),
                  DT_rows_all = list(), DT_rows_current = list()))
    }
    order_col <- NULL
    descending <- FALSE
    if (length(params$order) > 0) {
      ord <- params$order[[1]]
      k <- ord[["column"]]
      if (identical(params$columns[[k]][["orderable"]], "true")) {
        order_col <- state$cols[as.integer(k) + 1L]
        descending <- identical(ord[["dir"]], "desc")
      }
    }
    start <- as.integer(params$start)
    len <- as.integer(params$length)
    page <- fetch_table_page(state$query, state$cols, start, len, order_col, descending)

    # Escape HTML: context properties contain "<<description>>" text, which a
    # browser would otherwise swallow as an unknown tag
    for (j in seq_along(page)) {
      if (is.character(page[[j]])) page[[j]] <- htmltools::htmlEscape(page[[j]])
    }
    clean <- utils::getFromNamespace("cleanDataFrame", "DT")
    list(
      draw = draw,
      recordsTotal = state$total,
      recordsFiltered = state$total,
      data = clean(page),
      DT_rows_all = list(),
      DT_rows_current = if (nrow(page) > 0) start + seq_len(nrow(page)) else list()
    )
  }
}

#' data_table Server Functions
#'
#' The table uses DataTables server-side processing: every page and sort is a
#' DuckDB query, so any result size can be browsed and sorted without loading
#' it into R.
#'
#' @param id Internal parameter for {shiny}
#' @param filtered_query_cache reactiveVal holding the current lazy query
#' @param filtered_total reactiveVal holding the query's row count
#' @param columns_display_reactive Reactive vector of column names to display
#'
#' @noRd
mod_data_table_server <- function(id, filtered_query_cache, filtered_total, columns_display_reactive){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    state <- new.env(parent = emptyenv())
    state$query <- NULL
    state$cols <- character()
    state$total <- 0

    output$data_table <- DT::renderDT({
      query <- filtered_query_cache()
      total <- filtered_total()

      if (is.null(query) || is.null(total)) {
        state$query <- NULL
        return(DT::datatable(data.frame(), options = list(pageLength = 10)))
      }

      cols_to_show <- if (is.function(columns_display_reactive)) {
        isolate(columns_display_reactive())
      } else {
        columns_display_reactive
      }
      cols <- table_display_columns(colnames(query), cols_to_show)
      paged <- table_query(query, cols)
      state$query <- paged
      state$cols <- cols
      state$total <- total

      # Zero-row template: gives DT the column names and types
      template <- fetch_table_page(paged, cols, 0, 0)

      thin_cols <- which(!cols %in% c("taxon_name", "trait_name", "genus", "family"))
      # Species means: round for display (downloads keep full precision)
      numeric_cols <- if (is_species_means(colnames(query))) which(cols %in% c("value", "value_min", "value_max")) else integer()

      column_defs <- list(
        list(targets = thin_cols - 1, className = "truncated")
      )
      if (length(numeric_cols) > 0) {
        # Round a leading number to 3 significant figures (whole numbers
        # from 100 up), keeping any trailing unit. Fixed decimals would show
        # small means, such as seed masses, as 0.
        column_defs[[length(column_defs) + 1]] <- list(
          targets = numeric_cols - 1,
          render = DT::JS("function(data, type, row) {
            if (type === 'display' && data != null) {
              var m = String(data).match(/^(-?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)(.*)$/);
              if (!m) return data;
              var x = parseFloat(m[1]);
              var s = Math.abs(x) >= 100 ? x.toFixed(0) : String(Number(x.toPrecision(3)));
              return s + m[2];
            }
            return data;
          }")
        )
      }

      DT::datatable(
        data = template,
        escape = FALSE,  # escaped server-side in duckdb_table_filter()
        rownames = FALSE,
        filter = "none",
        class = "cell-border stripe nowrap",
        options = list(
          serverSide = TRUE,
          processing = TRUE,
          pageLength = 100,
          lengthMenu = c(10, 25, 50, 100),
          searching = FALSE,
          autoWidth = FALSE,
          scrollX = TRUE,
          scrollY = "500px",
          info = TRUE,
          headerCallback = DT::JS(
            "function(thead, data, start, end, display) {",
            "  $(thead).find('th').each(function(i) {",
            "    var col = this.textContent.trim();",
            "    var tips = {",
            "      value: 'Numeric traits: the observed mean (geometric, or arithmetic for a trait on a signed scale). Categorical traits: the most frequent value.',",
            "      value_count: 'Categorical traits: each value with the number of records carrying it.',",
            "      n_records: 'Number of records used.',",
            "      n_datasets: 'Number of datasets contributing records.',",
            "      method: 'How the value was found: geometric, arithmetic, mode or majority.',",
            "      reliability: 'Provisional class from the evidence behind the value: high, medium or low.'",
            "    };",
            "    if (tips[col]) $(this).attr('title', tips[col]);",
            "  });",
            "}"
          ),
          columnDefs = column_defs
        )
      )
    }, server = TRUE, funcFilter = duckdb_table_filter(state))

    invisible(NULL)
  })
}
