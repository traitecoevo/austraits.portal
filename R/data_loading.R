options(shiny.launch.browser = TRUE)

# Performance optimizations
#' Configure Shiny Application Options
#'
#' Sets global Shiny options to optimize application performance and resource handling:
#' - `shiny.autoreload = FALSE`: Disables automatic reloading to prevent interruptions during development
#' - `shiny.reactlog = FALSE`: Disables reactivity logging to reduce memory overhead and improve performance
#' - `shiny.maxRequestSize = 30 * 1024 ^ 2`: Sets maximum upload file size to 30 MB, preventing excessive memory consumption from large file uploads
options(
  shiny.autoreload = FALSE,
  shiny.reactlog = FALSE,
  shiny.maxRequestSize = 30 * 1024 ^ 2
)

# Data objects are created by load_portal_data(), which runs from .onLoad()
# (see R/zzz.R). Loading at package load time, rather than at the top level
# of this file, means the DuckDB connection is opened in the process that
# serves the app; a connection created at install time would be serialised
# into the package and be invalid when used. The placeholders below give the
# bindings a home in the namespace.
data_path <- NULL
duckdb_con <- NULL
austraits_display_duckdb <- NULL
austraits_species_display_duckdb <- NULL
austraits_duckdb <- NULL
austraits_species_duckdb <- NULL
sources <- NULL
trait_definitions <- NULL
trait_groups <- NULL
metatdata <- NULL
dropdowns <- NULL
flora_links <- NULL
taxon_synonyms <- NULL
generate_taxon_text_cached <- NULL

columns_display <- c(
  "dataset_id", "taxon_name", "genus", "family", "trait_name", "value", "unit",
  "entity_type", "value_type", "basis_of_value", "replicates", "basis_of_record",
  "life_stage", "collection_date", "measurement_remarks", "original_name",
  "location_name", "latitude (deg)", "longitude (deg)", "location_properties",
  "treatment_context_properties", "plot_context_properties",
  "entity_context_properties", "temporal_context_properties",
  "method_context_properties", "source_primary_citation", "data_contributors",
  "taxon_rank", "taxon_distribution", "establishment_means"
)

## Location
# TODO: Not yet implemented.
### Coordinates - circle/bbox around coordinates?

# Define controlled vocabulary columns (dropdown)
controlled_vocab_columns <- c(
  "dataset_id", "entity_type", "value_type", "basis_of_value",
  "basis_of_record", "life_stage", "original_name", "taxon_rank",
  "establishment_means"
)

# Define free-text columns (text input)
free_text_columns <- c(
  "collection_date", "measurement_remarks", "location_name",
  "location_properties", "treatment_context_properties",
  "plot_context_properties", "entity_context_properties",
  "temporal_context_properties", "method_context_properties",
  "source_primary_citation", "data_contributors"
)

# Define columns to omit from custom filter
omit_from_custom_filter <- c(
  "taxon_name", "genus", "family", "trait_name",
  "unit", "value", "replicates", "taxon_distribution",
  "latitude (deg)", "longitude (deg)"
)

# Available columns for custom filter
custom_filter_columns <- c(controlled_vocab_columns, free_text_columns)

# Custom filter columns for SPECIES AVERAGES (only columns that exist)
custom_filter_columns_species <- c(
  "dataset_id", "value_type", "value_count",
  "taxon_rank", "establishment_means"
  # Only controlled vocab columns that exist in species avg
)

# Custom Github hyperlink icon
target <- bsplus::shiny_iconlink(name = "github")
target$attribs$href <- "https://github.com/traitecoevo/austraits.portal"

# Columns to display for species averages (different from raw data)
columns_display_species <- c(
  "family", "genus", "taxon_name", "trait_name",
  "dataset_id",
  "value_mean", "value_range", "replicates",
  "taxon_rank", "taxon_distribution", "establishment_means"
)

#' Columns each data file must provide
#'
#' Columns the portal code reads. `required` columns stop the app at startup
#' if missing; for missing `optional` columns a message is printed and the
#' code falls back.
#' @noRd
data_schema <- list(
  display = list(
    required = unique(c(columns_display, "source_primary_key", "row_id",
      "taxonomic_status", "taxonomic_dataset", "binomial", "trinomial",
      "scientific_name", "taxon_id", "taxon_id_genus", "taxon_id_family",
      "scientific_name_id")),
    optional = character()
  ),
  species = list(
    required = c("taxon_name", "trait_name", "unit", "value_mean", "value_min",
      "value_max", "value_median", "dataset_id", "source_primary_key", "type",
      "row_id", "family", "genus"),
    optional = c("value_range")
  ),
  data = list(
    required = c("row_id", "source_primary_key", "genus"),
    optional = character()
  )
)

#' Check that a data table has the columns the portal reads
#'
#' @param columns Column names present in the table.
#' @param schema One element of `data_schema`.
#' @param label Name of the table, for messages.
#' @return Invisibly, the missing optional columns.
#' @noRd
check_data_schema <- function(columns, schema, label) {
  missing_required <- setdiff(schema$required, columns)
  if (length(missing_required) > 0) {
    stop(sprintf("Portal data '%s' is missing required columns: %s. Rebuild it with prepare_data_for_portal().",
                 label, paste(missing_required, collapse = ", ")), call. = FALSE)
  }
  missing_optional <- setdiff(schema$optional, columns)
  if (length(missing_optional) > 0) {
    packageStartupMessage(sprintf("Note: portal data '%s' is missing optional columns: %s. Some displays fall back to simpler output.",
                    label, paste(missing_optional, collapse = ", ")))
  }
  invisible(missing_optional)
}

#' Locate the portal data directory
#'
#' Prefers the full build over the lite one. Paths relative to the working
#' directory are used when the app runs from a checkout (`app.R`); the
#' installed package copies are used otherwise (e.g. under R CMD check).
#' @noRd
portal_data_path <- function() {
  full <- "extdata/austraits/austraits-7.0.0-full"
  lite <- "extdata/austraits/austraits-5.0.0-lite"
  candidates <- c(
    file.path("inst", full),
    system.file(full, package = "austraits.portal"),
    file.path("inst", lite),
    system.file(lite, package = "austraits.portal")
  )
  candidates <- candidates[nzchar(candidates) & dir.exists(candidates)]
  if (length(candidates) == 0) {
    stop("No portal data found under inst/extdata/austraits/", call. = FALSE)
  }
  if (!grepl("full", candidates[1])) {
    packageStartupMessage("Full dataset not found, loading lite dataset.")
  }
  candidates[1]
}

#' Load the portal data into the package namespace
#'
#' DuckDB reads the Parquet files directly (`read_parquet()` views) rather
#' than through Arrow: on Arrow-registered tables DuckDB cannot evaluate an
#' `IN` list with more than one value, which previously forced regex filters
#' that broke on taxon names containing brackets.
#'
#' @param env Environment to assign the data objects into.
#' @noRd
load_portal_data <- function(env) {
  # Absolute, so the views keep working if the working directory changes
  path <- normalizePath(portal_data_path())
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  view <- function(name, file, select = "*") {
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT %s FROM read_parquet(%s)",
      name, select, DBI::dbQuoteString(con, file.path(path, file))
    ))
  }
  # A hybrid formula such as "(Dockrillia pugioniformis x ...) x ..." gets the
  # genus "(Dockrillia" upstream (APCalign takes the name's first word); strip
  # the bracket so the taxon sits in its genus (issue #113)
  genus_sql <- "regexp_replace(genus, '^[(]', '') AS genus"
  # Coordinates are stored as text; cast them so location filters are numeric
  view("austraits_display", "austraits-display.parquet",
       paste0('* REPLACE (TRY_CAST("latitude (deg)" AS DOUBLE) AS "latitude (deg)", TRY_CAST("longitude (deg)" AS DOUBLE) AS "longitude (deg)", ', genus_sql, ')'))
  view("austraits_data", "austraits-data.parquet", paste0("* REPLACE (", genus_sql, ")"))
  # The species "display" file is a byte-identical copy, so one view serves both
  view("austraits_species", "austraits-species-averages.parquet", paste0("* REPLACE (", genus_sql, ")"))

  display <- dplyr::tbl(con, "austraits_display")
  data <- dplyr::tbl(con, "austraits_data")
  species <- dplyr::tbl(con, "austraits_species")

  check_data_schema(colnames(display), data_schema$display, "austraits-display.parquet")
  check_data_schema(colnames(species), data_schema$species, "austraits-species-averages.parquet")
  check_data_schema(colnames(data), data_schema$data, "austraits-data.parquet")

  metadata <- readRDS(file.path(path, "metadata.rds"))

  # Key the taxon-profile cache by data release and content, so a new data
  # build never serves profiles generated from the previous one
  version <- metadata$hits$hits[[1]]$metadata$version
  profile_inputs <- file.path(path, c("austraits-species-averages.parquet", "synonyms.rds"))
  profile_inputs <- profile_inputs[file.exists(profile_inputs)]
  fingerprint <- substr(digest_strings(unname(tools::md5sum(profile_inputs))), 1, 8)
  # .cache/ in a checkout (where app.R runs); a temporary directory otherwise,
  # so tests and R CMD check leave nothing behind
  cache_root <- getOption("austraits.portal.cache_dir",
    if (file.exists("app.R") && dir.exists("R")) ".cache" else file.path(tempdir(), "austraits-portal-cache"))
  cache_dir <- file.path(cache_root, "taxon_text", paste(version, fingerprint, sep = "-"))

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  assign("data_path", path, envir = env)
  assign("duckdb_con", con, envir = env)
  assign("austraits_display_duckdb", display, envir = env)
  assign("austraits_species_display_duckdb", species, envir = env)
  assign("austraits_duckdb", data, envir = env)
  assign("austraits_species_duckdb", species, envir = env)
  assign("sources", readRDS(file.path(path, "sources.rds")), envir = env)
  assign("trait_definitions", readRDS(file.path(path, "definitions.rds")), envir = env)
  assign("trait_groups", readRDS(file.path(path, "trait_groups.rds")), envir = env)
  assign("metatdata", metadata, envir = env)
  dropdowns <- readRDS(file.path(path, "dropdown_cache.rds"))
  dropdowns$all_genus <- sort(unique(sub("^[(]", "", dropdowns$all_genus)))
  assign("dropdowns", dropdowns, envir = env)
  assign("flora_links", readRDS(file.path(path, "flora_links.rds")), envir = env)
  # Optional: built by prepare_synonyms_for_portal(); Taxon View omits the
  # synonyms section without it
  synonyms_file <- file.path(path, "synonyms.rds")
  assign("taxon_synonyms", if (file.exists(synonyms_file)) readRDS(synonyms_file) else NULL, envir = env)
  assign("generate_taxon_text_cached",
         memoise::memoise(generate_taxon_text, cache = memoise::cache_filesystem(cache_dir)),
         envir = env)

  init_telemetry()
  invisible(path)
}

#' Read the sources bibliography (once per process)
#' @noRd
get_sources_bib <- memoise::memoise(function(path = data_path) {
  RefManageR::ReadBib(file = file.path(path, "sources.bib"), check = FALSE, .Encoding = "UTF-8")
})

#' Is telemetry switched off for this process?
#'
#' Off under testthat, Shiny test mode, or `AUSTRAITS_TELEMETRY=off`, so tests
#' and local runs never write to the production telemetry table.
#' @noRd
telemetry_disabled <- function() {
  identical(Sys.getenv("TESTTHAT"), "true") ||
    isTRUE(getOption("shiny.testmode")) ||
    identical(tolower(Sys.getenv("AUSTRAITS_TELEMETRY")), "off")
}

#' Configure telemetry: Supabase REST API, with SQLite fallback
#' @noRd
init_telemetry <- function() {
  if (telemetry_disabled()) {
    options(telemetry_mode = "off")
    return(invisible())
  }

  # Read credentials from config.yml (production)
  config_file <- "config.yml"

  if (file.exists(config_file)) {
    cfg <- config::get(config = "production", file = config_file)
    supabase_url <- cfg$supabase_url
    supabase_key <- cfg$supabase_key
  } else {
    # Fallback to environment variables
    supabase_url <- Sys.getenv("SUPABASE_URL")
    supabase_key <- Sys.getenv("SUPABASE_KEY")
  }

  if (nchar(supabase_url) > 0 && nchar(supabase_key) > 0) {
    packageStartupMessage("✓ Using Supabase telemetry (cloud)")
    init_supabase_telemetry(supabase_url, supabase_key)
    options(telemetry_mode = "cloud")
  } else {
    tryCatch({
      packageStartupMessage("✓ Using local SQLite telemetry")
      dir.create("inst/telemetry", showWarnings = FALSE, recursive = TRUE)
      telemetry <- shiny.telemetry::Telemetry$new(
        app_name = "austraits_portal",
        data_storage = shiny.telemetry::DataStorageSQLite$new(
          db_path = "inst/telemetry/telemetry.db"
        )
      )
      options(
        telemetry_mode = "local",
        telemetry_object = telemetry
      )
    }, error = function(e) {
      packageStartupMessage("ℹ Local telemetry not available (test environment)")
      options(telemetry_mode = "off")
    })
  }
}

#' Short hash of several hashes, so any input file's change changes the key
#' @noRd
digest_strings <- function(x) {
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(x, f)
  unname(tools::md5sum(f))
}
