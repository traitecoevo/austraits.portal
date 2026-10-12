# Concurrent-user load test: N headless browsers, each in its own R process,
# run the same scripted visit against a running app and time every step
# (open app, switch to raw data, filter to leaf_mass_per_area, Apply, table
# page 2, Trait View, back, download).
#
# Run the app the way Shiny Server does, with telemetry off, then:
#   AUSTRAITS_TELEMETRY=off Rscript -e 'shiny::runApp(".", port = 7102)'
#   Rscript dev/loadtest.R http://127.0.0.1:7102 branch 10 results.csv
# Needs chromote and a Chromium browser (set CHROMOTE_CHROME if it isn't
# found). Never point it at the live portal: it adds load for real users.
#
# Used instead of shinyloadtest, whose recording proxy stalls on this app
# after the first output message (issue #130).
#
# Usage: Rscript dev/loadtest.R <app_url> <label> <n_users> <out_csv>
args <- commandArgs(trailingOnly = TRUE)
url <- args[1]; label <- args[2]; n <- as.integer(args[3]); out_csv <- args[4]

visit <- function(url, user) {
  edge <- "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"
  if (!nzchar(Sys.getenv("CHROMOTE_CHROME")) && is.null(chromote::find_chrome()) && file.exists(edge)) {
    Sys.setenv(CHROMOTE_CHROME = edge)
  }
  b <- chromote::ChromoteSession$new(width = 1400, height = 1000)
  b$default_timeout <- 300
  js <- function(code) b$Runtime$evaluate(code, returnByValue = TRUE)$result$value
  # Track Shiny's busy/idle events and DataTables' ajax requests in the page
  hooks <- paste(
    "(function(){ if (!window.jQuery || !window.Shiny || !Shiny.shinyapp) return false;",
    "if (!window.__lt) { window.__lt = {busy: 0, seen: 0, dt: 0, last: Date.now()};",
    "$(document).on('shiny:busy', function(){ __lt.busy = 1; __lt.seen = 1; })",
    ".on('shiny:idle', function(){ __lt.busy = 0; __lt.last = Date.now(); })",
    ".on('shiny:value', function(){ __lt.last = Date.now(); })",
    ".on('preXhr.dt', function(){ __lt.dt = 1; __lt.seen = 1; })",
    ".on('xhr.dt', function(){ __lt.dt = 0; __lt.last = Date.now(); }); } return true; })()"
  )
  state_js <- "JSON.stringify(window.__lt ? [__lt.busy, __lt.seen, __lt.dt, Date.now() - __lt.last, Shiny.shinyapp.isConnected() ? 1 : 0] : null)"
  # Seconds from the action until the app is idle: no Shiny or table request
  # outstanding and no output update for 0.5 s. Steps that trigger no server
  # work count as done after 1 s with nothing seen.
  wait <- function(t0, timeout = 300) {
    repeat {
      elapsed <- as.numeric(Sys.time() - t0, units = "secs")
      if (elapsed > timeout) return(NA_real_)
      st <- js(state_js)
      if (!is.null(st)) {
        st <- jsonlite::fromJSON(st)
        idle <- st[1] == 0 && st[3] == 0 && st[5] == 1 && st[4] >= 500
        if (idle && (st[2] == 1 || elapsed > 1)) {
          return(max(0, elapsed - st[4] / 1000))
        }
      }
      Sys.sleep(0.1)
    }
  }
  timings <- list()
  step <- function(name, code, think) {
    js("window.__lt.seen = 0; true")
    t0 <- Sys.time(); js(code)
    timings[[name]] <<- wait(t0)
    Sys.sleep(think)
  }

  t0 <- Sys.time()
  invisible(b$Page$navigate(url, wait_ = FALSE))
  # Install the hooks as soon as Shiny's page is up, then wait for the first load
  for (i in 1:600) { if (isTRUE(tryCatch(js(hooks), error = function(e) FALSE))) break; Sys.sleep(0.1) }
  js("window.__lt.seen = 1; true")
  timings[["open app"]] <- wait(t0)
  Sys.sleep(3)
  step("raw data", "$('input[name=\"filters-dataset_type\"][value=\"raw\"]').click()", think = 2)
  js("Shiny.setInputValue('filters-trait_name', ['leaf_mass_per_area'], {priority:'event'})"); Sys.sleep(1)
  step("apply filter", "$('#filters-apply_filters_btn').click()", think = 3)
  step("table page 2", "$('#data_table-data_table .paginate_button.next, #data_table-data_table .dt-paging-button.next').first().click()", think = 3)
  step("trait view", "$('a[data-value=\"Trait View\"]').click()", think = 3)
  step("back to table", "$('a[data-value=\"Data Preview\"]').click()", think = 2)

  href <- js("$('#filters-download_data').attr('href')")
  dl <- tempfile(fileext = ".zip")
  t0 <- Sys.time()
  ok <- tryCatch({ utils::download.file(paste0(url, "/", href), dl, quiet = TRUE, mode = "wb"); TRUE }, error = function(e) { message("download failed: ", conditionMessage(e), " href=", href); FALSE })
  timings[["download"]] <- if (ok && file.size(dl) > 1e5) as.numeric(Sys.time() - t0, units = "secs") else NA_real_
  if (ok) message("user ", user, " download bytes: ", file.size(dl))
  b$close()
  data.frame(user = user, step = names(timings), seconds = unlist(timings))
}

workers <- lapply(seq_len(n), function(i) {
  Sys.sleep(if (i == 1) 0 else 2)   # users arrive 2 s apart
  callr::r_bg(visit, args = list(url = url, user = i))
})
results <- do.call(rbind, lapply(workers, function(w) { w$wait(); w$get_result() }))
results$version <- label
results$users <- n
utils::write.csv(results, out_csv, row.names = FALSE)
agg <- aggregate(seconds ~ step, results, function(x) c(median = median(x, na.rm = TRUE), max = max(x, na.rm = TRUE)))
order <- c("open app", "raw data", "apply filter", "table page 2", "trait view", "back to table", "download")
agg <- agg[match(order, agg$step), ]
cat(sprintf("%s, %d users\n", label, n))
for (i in seq_len(nrow(agg))) cat(sprintf("  %-14s median %5.1f s   max %5.1f s\n", agg$step[i], agg$seconds[i, "median"], agg$seconds[i, "max"]))
