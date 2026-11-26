#' File name stem for a download
#'
#' Names every download after the AusTraits release it came from, e.g.
#' `austraits-8.0.0-data` or `austraits-8.0.0-species-means`.
#' @param release Release record (see `portal_release_info()`).
#' @param dataset_type "raw" or "species".
#' @noRd
download_stem <- function(release, dataset_type) {
  sprintf("austraits-%s-%s", release$version,
          if (identical(dataset_type, "species")) "species-means" else "data")
}

#' R script that recreates a download with the austraits package
#'
#' @description Writes the R code that loads the same AusTraits release, pinned
#'   by its DOI, and applies the same filters as the portal. The filters are
#'   printed from [filter_expressions()], the code the portal itself runs, so
#'   the script selects the same rows.
#' @param parsed_filters Parsed filter object from parse_filters().
#' @param dataset_type "raw" or "species".
#' @param release Release record (see `portal_release_info()`).
#' @param columns Column names of the portal table the filters ran on.
#' @param date Date of the download, for the header.
#' @param load Include the lines that install the package and load the data?
#'   FALSE leaves them out, so tests can supply `austraits` or `means`.
#' @return Character vector, one element per line.
#' @keywords internal
#' @noRd
recreate_script <- function(parsed_filters, dataset_type, release, columns,
                            date = Sys.Date(), load = TRUE) {
  species <- identical(dataset_type, "species")
  expressions <- filter_expressions(parsed_filters, columns)
  used <- unique(unlist(lapply(expressions, all.vars)))

  doi_text <- if (is.na(release$doi)) "no Zenodo record (development build)" else sprintf("doi:%s", release$doi)
  load_arg <- if (is.na(release$doi)) sprintf('version = "%s"', release$version) else sprintf('doi = "%s"', release$doi)
  package_ref <- if (is.na(release$austraits_package)) "traitecoevo/austraits" else
    sprintf("traitecoevo/austraits@%s", release$austraits_package)

  header <- c(
    sprintf("# Recreates %s.csv, downloaded from the AusTraits Data Portal on %s.",
            download_stem(release, dataset_type), format(date)),
    sprintf("# Data: AusTraits %s, %s", release$version, doi_text),
    "# The filters below are the ones the portal applied.",
    if (species) {
      "# Species means are the observed means published with the release; see its observed-means dictionary for each column."
    },
    if (species) {
      "# Names that APCalign could not align to the APC (empty taxon_rank) are left out, as in the portal."
    },
    "# The portal's file also carries source_primary_key, source_primary_citation and row_id, which it adds for citations.",
    ""
  )

  install <- c(
    "# install.packages(\"remotes\")",
    sprintf("# remotes::install_github(\"%s\")", package_ref),
    ""
  )

  needs_database <- !species || "taxon_distribution" %in% used
  needs_citation <- !species && any(c("source_primary_citation", "source_primary_key") %in% used)
  georeferenced <- any(c("latitude (deg)", "longitude (deg)") %in% used)

  load_lines <- c(
    if (needs_database) sprintf("austraits <- austraits::load_austraits(%s)", load_arg),
    if (species) sprintf("means <- austraits::load_austraits(%s, means = TRUE)", load_arg),
    ""
  )

  # Each step the portal applies to the published tables before filtering
  steps <- character()
  if (species) {
    source_object <- "means"
    steps <- c(steps,
      "  # The portal leaves out names APCalign could not align to the APC",
      "  dplyr::filter(!is.na(taxon_rank)) |>")
    if ("taxon_distribution" %in% used) {
      steps <- c(steps,
        "  # The APC distribution, from the database's taxa table",
        "  dplyr::left_join(",
        "    dplyr::distinct(dplyr::select(austraits$taxa, taxon_name, taxon_distribution), taxon_name, .keep_all = TRUE),",
        "    by = \"taxon_name\"",
        "  ) |>")
    }
  } else {
    source_object <- "austraits"
    steps <- c(steps, "  austraits::flatten_database() |>")
    if (needs_citation) {
      # As join_primary_source() for traits.build 2.3 and later (every release
      # from 8.0.0); older builds could cite another key from their methods
      steps <- c(steps,
        "  # Each dataset's own reference, as the portal names it",
        "  dplyr::mutate(source_primary_key = dataset_id, source_primary_citation = dataset_id_citation) |>")
    }
  }
  steps <- c(steps,
    "  # Hybrid formulas \"(Genus species x ...)\" carry the genus \"(Genus\"; the portal strips the bracket",
    "  dplyr::mutate(genus = sub(\"^[(]\", \"\", genus)) |>")
  if (georeferenced) {
    steps <- c(steps,
      "  # Coordinates are stored as text; the portal compares them as numbers",
      "  dplyr::mutate(",
      "    `latitude (deg)` = suppressWarnings(as.numeric(`latitude (deg)`)),",
      "    `longitude (deg)` = suppressWarnings(as.numeric(`longitude (deg)`))",
      "  ) |>")
  }

  filter_lines <- if (length(expressions) == 0) {
    "  dplyr::filter()  # no filters: the whole table"
  } else {
    deparsed <- lapply(expressions, function(e) {
      # Base deparse() prints every value; rlang's abbreviates long vectors
      lines <- deparse(e, width.cutoff = 70)
      paste0("    ", lines)
    })
    # Separate the conditions with commas (dplyr::filter() combines them with &)
    for (i in seq_len(length(deparsed) - 1)) {
      n <- length(deparsed[[i]])
      deparsed[[i]][n] <- paste0(deparsed[[i]][n], ",")
    }
    c("  dplyr::filter(", unlist(deparsed), "  )")
  }

  c(
    header,
    if (load) c(install, load_lines),
    sprintf("data <- %s |>", source_object),
    steps,
    filter_lines,
    ""
  )
}
