#' Supabase REST API Telemetry Functions
#' Uses HTTP instead of PostgreSQL driver
#' Auto-detects cloud (Supabase) vs local (SQLite) mode

#' Initialize Supabase Telemetry
#' @param url Supabase project URL.
#' @param key Supabase service or anon API key.
init_supabase_telemetry <- function(url, key) {
  options(supabase_telemetry_url = url)
  options(supabase_telemetry_key = key)
  packageStartupMessage("✓ Supabase telemetry initialized")
}

#' Send one telemetry event to Supabase without blocking the R process
#'
#' The POST runs asynchronously on the later event loop, so a slow or
#' unreachable Supabase never stalls the (single-process) app for other users.
#' @param body Named list sent as the JSON body.
#' @noRd
send_supabase_event <- function(body) {
  url <- getOption("supabase_telemetry_url")
  key <- getOption("supabase_telemetry_key")
  if (is.null(url) || is.null(key)) return(invisible(NULL))

  req <- httr2::request(paste0(url, "/rest/v1/telemetry_events")) |>
    httr2::req_headers(
      "apikey" = key,
      "Authorization" = paste("Bearer", key),
      "Content-Type" = "application/json",
      "Prefer" = "return=minimal"
    ) |>
    httr2::req_body_json(body)

  tryCatch(
    promises::catch(httr2::req_perform_promise(req), function(e) {
      warning("Telemetry send failed (", body$type, "): ", conditionMessage(e), call. = FALSE)
    }),
    error = function(e) {
      warning("Telemetry send failed (", body$type, "): ", conditionMessage(e), call. = FALSE)
    }
  )
  invisible(NULL)
}

#' Start telemetry session (works in both cloud and local mode)
start_telemetry_session <- function() {
  if (telemetry_disabled()) return(invisible(NULL))
  mode <- getOption("telemetry_mode", "off")
  if (mode == "local") {
    # Local SQLite mode
    telemetry_obj <- getOption("telemetry_object")
    if (!is.null(telemetry_obj)) {
      telemetry_obj$start_session(track_inputs = FALSE)
    }
  } else if (mode == "cloud") {
    send_supabase_event(list(
      type = "login",
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      session_id = paste0("session_", as.numeric(Sys.time()))
    ))
  }
}

#' Log telemetry event (works in both cloud and local mode)
#' @param event_type Event type label.
#' @param details Named list of event details.
log_telemetry_event <- function(event_type, details = list()) {
  if (telemetry_disabled()) return(invisible(NULL))
  mode <- getOption("telemetry_mode", "off")
  if (mode == "local") {
    # Local SQLite mode
    telemetry_obj <- getOption("telemetry_object")
    if (!is.null(telemetry_obj)) {
      telemetry_obj$log_custom_event(event_type, details = details)
    }
  } else if (mode == "cloud") {
    send_supabase_event(list(
      type = event_type,
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      details = if (length(details) > 0) details else NULL
    ))
  }
}

#' Read telemetry metrics (works in both cloud and local mode)
#' @param from_date Start date (YYYY-MM-DD) for telemetry records.
#' @param to_date End date (YYYY-MM-DD) for telemetry records.
read_telemetry_metrics <- function(from_date = "2020-01-01", to_date = NULL) {
  mode <- getOption("telemetry_mode", "off")
  if (mode == "off") {
    return(tibble::tibble(type = character(), timestamp = character(), details = character()))
  }
  if (mode == "local") {
    # Local SQLite mode
    telemetry_obj <- getOption("telemetry_object")
    if (is.null(telemetry_obj)) {
      return(tibble::tibble(type = character(), timestamp = character(), details = character()))
    }
    
    tryCatch({
      data <- telemetry_obj$data_storage$read_event_data(
        from_date, 
        if(is.null(to_date)) as.character(Sys.Date() + 1) else to_date
      )
      return(data)
    }, error = function(e) {
      return(tibble::tibble(type = character(), timestamp = character(), details = character()))
    })
  } else {
    # Cloud Supabase mode - WITH PAGINATION
    url <- getOption("supabase_telemetry_url")
    key <- getOption("supabase_telemetry_key")
    
    if (is.null(url) || is.null(key)) {
      return(tibble::tibble(type = character(), timestamp = character(), details = character()))
    }
    
    if (is.null(to_date)) to_date <- as.character(Sys.Date() + 1)
    
    tryCatch({
      all_data <- list()
      offset <- 0
      
      repeat {
       
        query_url <- paste0(
          url, 
          "/rest/v1/telemetry_events?",
          "timestamp=gte.", from_date,
          "&timestamp=lte.", to_date,
          "&order=timestamp.desc",
          "&limit=1000",
          "&offset=", offset
        )
      
        resp <- httr2::request(query_url) |>
          httr2::req_headers(
            "apikey" = key,
            "Authorization" = paste("Bearer", key)
          ) |>
          httr2::req_perform() |>
          httr2::resp_body_json()
        
        if (length(resp) == 0) break
        
        all_data[[length(all_data) + 1]] <- resp
        offset <- offset + 1000
      }
      
      # Combine all batches
      resp <- unlist(all_data, recursive = FALSE)
      
      if (length(resp) > 0) {
        tibble::tibble(
          type = sapply(resp, function(x) if(is.null(x$type)) NA else x$type),
          timestamp = sapply(resp, function(x) if(is.null(x$timestamp)) NA else x$timestamp),
          details = sapply(resp, function(x) {
            if (!is.null(x$details)) jsonlite::toJSON(x$details, auto_unbox = TRUE) else ""
          })
        )
      } else {
        tibble::tibble(type = character(), timestamp = character(), details = character())
      }
    }, error = function(e) {
      warning("Telemetry read failed: ", e$message)
      tibble::tibble(type = character(), timestamp = character(), details = character())
    })
  }
}
#' Monthly event counts (month, type, n)
#'
#' Cloud mode reads the telemetry_monthly view, so the key never needs to
#' read raw rows. Other modes count the raw rows in R.
#' @noRd
read_telemetry_monthly <- function() {
  empty <- tibble::tibble(month = character(), type = character(), n = numeric())
  mode <- getOption("telemetry_mode", "off")
  if (mode == "off") return(empty)

  if (mode == "cloud") {
    url <- getOption("supabase_telemetry_url")
    key <- getOption("supabase_telemetry_key")
    if (is.null(url) || is.null(key)) return(empty)
    return(tryCatch({
      resp <- httr2::request(paste0(url, "/rest/v1/telemetry_monthly?select=month,type,n&limit=10000")) |>
        httr2::req_headers("apikey" = key, "Authorization" = paste("Bearer", key)) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
      if (length(resp) == 0) return(empty)
      tibble::tibble(
        month = vapply(resp, function(x) if (is.null(x$month)) NA_character_ else x$month, ""),
        type  = vapply(resp, function(x) if (is.null(x$type)) NA_character_ else x$type, ""),
        n     = vapply(resp, function(x) as.numeric(x$n), numeric(1))
      )
    }, error = function(e) {
      warning("Telemetry read failed: ", conditionMessage(e), call. = FALSE)
      empty
    }))
  }

  raw <- read_telemetry_metrics()
  if (nrow(raw) == 0) return(empty)
  raw |>
    dplyr::mutate(month = format(as.Date(timestamp), "%Y-%m")) |>
    dplyr::count(month, type, name = "n")
}

read_telemetry_monthly_cached <- memoise::memoise(
  function() read_telemetry_monthly(),
  cache = cachem::cache_mem(max_age = 60)
)
#' Telemetry rows, cached for 60 seconds across all sessions
#'
#' The App Information tab shows totals and a monthly table, refreshed every
#' minute. Without this cache each open tab paged through the whole telemetry
#' table, twice, every minute.
#' @noRd
read_telemetry_metrics_cached <- memoise::memoise(
  function(from_date, to_date) read_telemetry_metrics(from_date, to_date),
  cache = cachem::cache_mem(max_age = 60)
)

#' Get monthly telemetry metrics
#' @param from_date Start date
#' @param to_date End date
  
get_monthly_metrics <- function(from_date = "2020-01-01", to_date = NULL) {
  data <- read_telemetry_monthly_cached()

  if (nrow(data) == 0) {
    return(tibble::tibble(month = character(), logins = numeric(),
                          searches = numeric(), downloads = numeric()))
  }

  data |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      logins = sum(n[type == "login"], na.rm = TRUE),
      searches = sum(n[type == "search"], na.rm = TRUE),
      downloads = sum(n[type == "download"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(month))
}
#' Export COUNTER metrics as CSV
export_counter_csv <- function(from_date = "2020-01-01", to_date = NULL) {
  monthly_data <- get_monthly_metrics(from_date, to_date)
  
  if (nrow(monthly_data) == 0) {
    return(NULL)
  }
  
  counter_df <- data.frame(
    Reporting_Period = monthly_data$month,
    Metric_Type_Investigations = "Investigations",
    Investigations_Count = monthly_data$logins,
    Metric_Type_Requests = "Requests",
    Requests_Count = monthly_data$searches + monthly_data$downloads,
    Searches = monthly_data$searches,
    Downloads = monthly_data$downloads
  )
  
  return(counter_df)
}

#' Export COUNTER metrics as JSON
export_counter_json <- function(from_date = "2020-01-01", to_date = NULL) {
  monthly_data <- get_monthly_metrics(from_date, to_date)
  
  if (nrow(monthly_data) == 0) {
    return(NULL)
  }
  
  counter_list <- lapply(1:nrow(monthly_data), function(i) {
    row <- monthly_data[i, ]
    list(
      reporting_period = row$month,
      investigations = as.numeric(row$logins),
      requests_total = as.numeric(row$searches) + as.numeric(row$downloads),
      searches = as.numeric(row$searches),
      downloads = as.numeric(row$downloads)
    )
  })
  
  return(jsonlite::toJSON(counter_list, pretty = TRUE))
}
