#' Build the synonym table for Taxon View
#'
#' @description For each taxon in the portal data, lists the other names the
#'   Australian Plant Census (APC) links to its accepted name: synonyms and
#'   misapplied names. Precomputed at data-build time with APCalign, so the
#'   app never loads the APC itself (issue #112).
#'
#'   AusTraits taxa are matched to APC accepted names by taxon ID first, then
#'   by name, since some IDs change between APC releases.
#'
#' @param taxa Data frame with `taxon_name` and `taxon_id`, one row per taxon.
#' @param resources Output of `APCalign::load_taxonomic_resources()`.
#' @param apc_version APC release used, stored as the `apc_version` attribute.
#' @return Data frame with `taxon_name`, `name`, `scientific_name` (with
#'   authority), `taxonomic_status` and `kind` ("synonym" or "misapplied").
#' @keywords internal
#' @noRd
build_taxon_synonyms <- function(taxa, resources, apc_version = NA_character_) {
  apc <- resources$APC
  accepted <- apc[apc$taxonomic_status == "accepted", c("taxon_ID", "canonical_name")]

  taxa <- unique(taxa[, c("taxon_name", "taxon_id")])
  id_by_name <- accepted$taxon_ID[match(taxa$taxon_name, accepted$canonical_name)]
  taxa$accepted_ID <- ifelse(taxa$taxon_id %in% accepted$taxon_ID, taxa$taxon_id, id_by_name)
  taxa <- taxa[!is.na(taxa$accepted_ID), c("taxon_name", "accepted_ID")]

  others <- apc[!apc$taxonomic_status %in% c("accepted", "excluded") &
                  apc$accepted_name_usage_ID %in% taxa$accepted_ID,
                c("accepted_name_usage_ID", "canonical_name", "scientific_name", "taxonomic_status")]

  out <- merge(taxa, others, by.x = "accepted_ID", by.y = "accepted_name_usage_ID")
  out <- data.frame(
    taxon_name = out$taxon_name,
    name = out$canonical_name,
    scientific_name = out$scientific_name,
    taxonomic_status = out$taxonomic_status,
    kind = ifelse(grepl("misapplied", out$taxonomic_status), "misapplied", "synonym"),
    stringsAsFactors = FALSE
  )
  out <- out[out$name != out$taxon_name, ]
  out <- unique(out[order(out$taxon_name, out$kind, out$name), ])
  rownames(out) <- NULL
  attr(out, "apc_version") <- apc_version
  out
}

#' Write the synonym table for a portal data directory
#'
#' Reads the taxa from the directory's display Parquet file and writes
#' `synonyms.rds` beside it. Runs on its own for an existing data build, and
#' from `prepare_data_for_portal()` for a new one.
#'
#' @param output_dir Portal data directory.
#' @param version APC release to use; defaults to APCalign's default.
#' @keywords internal
#' @noRd
prepare_synonyms_for_portal <- function(output_dir, version = NULL) {
  if (!requireNamespace("APCalign", quietly = TRUE)) {
    stop("prepare_synonyms_for_portal() needs the APCalign package.", call. = FALSE)
  }
  if (is.null(version)) version <- APCalign::default_version()
  resources <- APCalign::load_taxonomic_resources(version = version, quiet = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  taxa <- DBI::dbGetQuery(con, sprintf(
    "SELECT DISTINCT taxon_name, taxon_id FROM read_parquet(%s) WHERE taxon_name IS NOT NULL",
    DBI::dbQuoteString(con, file.path(output_dir, "austraits-display.parquet"))
  ))

  synonyms <- build_taxon_synonyms(taxa, resources, apc_version = version)
  saveRDS(synonyms, file.path(output_dir, "synonyms.rds"))
  message(sprintf("✓ Synonyms saved: %d names for %d taxa (APC %s)",
                  nrow(synonyms), length(unique(synonyms$taxon_name)), version))
  invisible(synonyms)
}
