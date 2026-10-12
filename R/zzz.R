.onLoad <- function(libname, pkgname) {
  load_portal_data(topenv())
}

.onUnload <- function(libpath) {
  con <- get0("duckdb_con", envir = topenv())
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
}
