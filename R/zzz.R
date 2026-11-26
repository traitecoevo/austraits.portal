.onLoad <- function(libname, pkgname) {
  # Data preparation needs the package's functions before any portal data
  # exists in the current layout; it sets AUSTRAITS_PORTAL_NO_DATA=true
  if (identical(Sys.getenv("AUSTRAITS_PORTAL_NO_DATA"), "true")) return(invisible())
  load_portal_data(topenv())
}

.onUnload <- function(libpath) {
  con <- get0("duckdb_con", envir = topenv())
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
}
