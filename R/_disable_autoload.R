# This file stops Shiny from auto-sourcing R/ when Shiny Server (or
# shiny::runApp()) runs app.R. app.R loads this directory as a package with
# pkgload::load_all(), and the data are loaded by the package's .onLoad().
# Auto-sourced copies of R/ would shadow the package's functions with copies
# that see only the empty data placeholders in R/data_loading.R.
# The leading underscore keeps this file out of the package build.
