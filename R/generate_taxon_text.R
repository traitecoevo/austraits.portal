
#' Generate Text Summary for a Taxon
#'
#' Creates a textual summary for a specified taxon based on the provided data.
#'
#' @param taxon A character string specifying the name of the taxon to summarize.
#'
#' @return A character string containing the generated summary for the specified taxon.
#'
#' @examples
#' \dontrun{
#' generate_taxon_text("Eucalyptus globulus")
#' }

generate_taxon_text <- function(taxon) {

  data_taxon <- austraits_display_duckdb |>
    dplyr::filter(taxon_name == !!taxon) |>
    dplyr::collect()

  # The published means roll subspecies, varieties and forms up to their
  # species; an infraspecific taxon with no means of its own shows its
  # species'. A taxon's own means come first: a hybrid formula such as
  # "Acacia adoxa var. adoxa x Acacia spondylophylla" has its own row.
  means_taxon <- taxon
  data_taxon_trait_means <- austraits_species_duckdb |>
    dplyr::filter(taxon_name == !!taxon) |>
    dplyr::collect()
  if (nrow(data_taxon_trait_means) == 0 && species_means_name(taxon) != taxon) {
    means_taxon <- species_means_name(taxon)
    data_taxon_trait_means <- austraits_species_duckdb |>
      dplyr::filter(taxon_name == !!means_taxon) |>
      dplyr::collect()
  }

  taxon_info <- data_taxon |>
    dplyr::select(
      taxon_name, 
      taxon_distribution, 
      taxon_rank, 
      taxonomic_status, 
      taxonomic_dataset,
      genus, 
      family, 
      binomial,
      trinomial, 
      establishment_means,
      scientific_name,
      taxon_id,
      taxon_id_genus, 
      taxon_id_family,
      scientific_name_id
    ) |>
    dplyr::slice(1) |>
    as.list()

  # Generate links to other portals
  portal_links <- generate_taxon_portal_links(taxon_info)

  # Every trait with a mean for the taxon, grouped as in trait_groups; core
  # traits first, then the others
  groups <- trait_groups |>
    dplyr::rename(trait_name = trait) |>
    dplyr::select("trait_name", "Entity", "trait_group_for_portal", "core_trait")
  trait_info_have_data <-
    data_taxon_trait_means |>
    dplyr::left_join(groups, by = "trait_name") |>
    dplyr::mutate(
      core_trait = ifelse(!is.na(core_trait) & core_trait == "core_trait", "core", "other"),
      trait_group_for_portal = dplyr::coalesce(trait_group_for_portal, "other traits"),
      # Traits missing from trait_groups link to their definition's URI
      Entity = dplyr::coalesce(Entity, vapply(trait_name, function(t) {
        uri <- trait_definitions[[t]]$entity_URI
        if (is.null(uri)) NA_character_ else uri
      }, character(1)))
    ) |>
    dplyr::mutate(text =
      ifelse(type == "categorical",
        sprintf("- [%s](%s) (categorical): %s  [sources: %s]", trait_name, Entity, value_count, dataset_id),
        sprintf("- [%s](%s) (numerical, %s mean of %s records): %s %s (%s-%s %s)  [sources: %s]",
          trait_name, Entity, method, n_records,
          format_sig3(value), dplyr::coalesce(unit, ""),
          format_sig3(value_min), format_sig3(value_max), dplyr::coalesce(unit, ""),
          dataset_id)
      )) |>
    dplyr::group_by(trait_group_for_portal, core_trait) |>
    dplyr::summarise(
      text = paste(text, collapse = "\n"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      text2 = sprintf(
        "<details> <summary> %s </summary>\n\n %s </details>", trait_group_for_portal, text)
    ) |>
    split(~core_trait)

  data_taxon_summary_dataset <-
    data_taxon |>
    dplyr::group_by(dataset_id) |>
    dplyr::summarise(
      n_traits = dplyr::n_distinct(trait_name),
      n_records = n(),
      reference = source_primary_citation[1],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      text = sprintf("%s (%s)", dataset_id, n_records) |> paste(collapse = ", ")
    ) |>
    dplyr::relocate(reference, .after = text) |>
    dplyr::arrange(desc(n_records))

  data_taxon_summary_dataset |> dplyr::slice_head(n=1) |> dplyr::pull(text) |> paste(collapse = ", ")

  # Generate the taxon description for display
  taxon_description <- 
    sprintf(
"## %s

**FAMILY** %s

**Taxonomy**: %s

%s**Distribution (APC)**: %s

**Profiles**: %s

## Traits 

**Top source datasets**: %s.     See below for a full list of sources.<br>
**Summary**: AusTraits contains %s traits from %s records in %s datasets.<br>

**Traits by category** (click to expand):<br>

- Species-level observed means from AusTraits %s. For categorical traits, the number of records for each value is shown. For numerical traits, the mean and range are shown.%s<br>

%s

%s

## Sources 


", 
    taxon,
    taxon_info$family, 
    taxon_info$scientific_name, 
    format_taxon_synonyms(taxon),
    taxon_info$taxon_distribution,
    sprintf("[%s](%s)", portal_links$source, portal_links$url) |> paste(collapse = ", "),
    data_taxon_summary_dataset |> dplyr::slice(1) |> dplyr::pull(text) |> paste(collapse = ", "), 
    dplyr::n_distinct(data_taxon$trait_name),
    nrow(data_taxon),
    dplyr::n_distinct(data_taxon$dataset_id), 
    release$version,
    if (means_taxon != taxon) sprintf(" Means are for the species *%s*, which includes this taxon's records.", means_taxon) else "",
    trait_info_have_data$core$text2 |> paste(collapse = "\n\n"),
    if (length(trait_info_have_data$other$text2) > 0) {
      paste0("**Other traits** (click to expand):<br>\n\n", paste(trait_info_have_data$other$text2, collapse = "\n\n"))
    } else ""
    )

  sources <- 
    data_taxon_summary_dataset |>
    dplyr::mutate(
      # convert reference column to html
      reference = purrr::map_chr(reference,     
          ~commonmark::markdown_html(.x) )
    ) |>
    dplyr::select(-text) |>
    # convert to html
    kableExtra::kable(format = "html") |>
    kableExtra::kable_styling("striped", full_width = F) |>
    kableExtra::column_spec(1, width = "10em") |>
    kableExtra::column_spec(2, width = "5em") |>
    # Clean up html
    stringr::str_remove_all("<p>") |> 
    stringr::str_remove_all("</p>") |>
    stringr::str_replace_all("&lt;", "<") |>
    stringr::str_replace_all("&gt;", ">")

  result <- c(add_target_blank(taxon_description), add_target_blank(sources))
  
  return(result)
}

#' Generate Portal Links for a Taxon
#' @keywords internal
#' @noRd 
generate_taxon_portal_links <- function(taxon_info) {
  # Get distribution and taxon name
  distribution <- if (is.null(taxon_info$taxon_distribution)) "" else taxon_info$taxon_distribution
  taxon <- taxon_info$taxon_name
  
  # PERMANENT LINKS (always show, in specified order)
  links <- dplyr::tribble(
    ~source, ~url,
    "APC", taxon_info$taxon_id,
    "Flora of Australia", sprintf("https://profiles.ala.org.au/opus/foa/profile/%s", gsub(" ", "%20", taxon)),
    "ALA", sprintf("https://bie.ala.org.au/species/%s", taxon_info$taxon_id),
    "iNaturalist", sprintf("https://www.inaturalist.org/taxa/search?q=%s", 
      gsub(" ", "-", gsub(" var\\. | subsp\\. ", " ", taxon)))
  )
  
  # STATE/REGIONAL LINKS (conditional based on distribution)
  
  # NSW Flora only if distributed in NSW
  if (grepl("NSW", distribution, ignore.case = TRUE)) {
    links <- links |> 
      dplyr::add_row(
        source = "NSW Flora",
        url = sprintf("https://plantnet.rbgsyd.nsw.gov.au/cgi-bin/NSWfl.pl?page=nswfl&lvl=sp&name=%s", 
                     gsub(" ", "~", taxon))
      )
  }
  
  # NT eFlora only if distributed in NT
  if (grepl("NT", distribution, ignore.case = TRUE)) {
    # Try to find in CSV
    nt_match <- flora_links$nt |> dplyr::filter(taxon_name == taxon)
    
    if (nrow(nt_match) > 0 && !is.na(nt_match$url[1])) {
      # Use URL from CSV
      links <- links |>
        dplyr::add_row(
          source = "NT eFlora",
          url = nt_match$url[1]
        )
    } else {
      # Default to homepage
      links <- links |>
        dplyr::add_row(
          source = "NT eFlora",
          url = "https://eflora.nt.gov.au/"
        )
    }
  }
  
  # SA eFlora only if distributed in SA (homepage until we get numeric IDs)
  if (grepl("SA", distribution, ignore.case = TRUE)) {
    links <- links |>
      dplyr::add_row(
        source = "SA eFlora",
        url = "https://flora.sa.gov.au/"
      )
  }
  
  # Vic Flora only if distributed in Vic/Victoria
  if (grepl("Vic", distribution, ignore.case = TRUE)) {
    # Try to find in CSV
    vic_match <- flora_links$vic |> dplyr::filter(taxon_name == taxon)
    
    if (nrow(vic_match) > 0 && !is.na(vic_match$url[1])) {
      # Use URL from CSV
      links <- links |>
        dplyr::add_row(
          source = "Vic Flora",
          url = vic_match$url[1]
        )
    } else {
      # Default to homepage
      links <- links |>
        dplyr::add_row(
          source = "Vic Flora",
          url = "https://vicflora.rbg.vic.gov.au/flora/taxon/"
        )
    }
  }
  
  # Florabase only if distributed in WA (homepage until we get numeric IDs)
  if (grepl("WA", distribution, ignore.case = TRUE)) {
    links <- links |>
      dplyr::add_row(
        source = "Florabase",
        url = "https://florabase.dbca.wa.gov.au/"
      )
  }
  
  # ATRP only if distributed in Qld (Australian Tropical Rainforest Plants)
    atrp_match <- flora_links$atrp |> dplyr::filter(taxon_name == taxon)
    
    if (nrow(atrp_match) > 0)  {
      # Use URL from CSV
      links <- links |>
        dplyr::add_row(
          source = "ATRP",
          url = atrp_match$url[1]
        )
    } else {
      # Default to homepage
      links <- links |>
        dplyr::add_row(
          source = "ATRP",
          url = "https://apps.lucidcentral.org/rainforest/"
        )
    }
  
  return(links)
}

#' Synonyms section of a taxon profile
#'
#' Markdown listing the APC synonyms and misapplied names of a taxon, from the
#' table built by prepare_synonyms_for_portal() (issue #112). Empty when the
#' data build has no synonym table.
#'
#' @param taxon Taxon name.
#' @param synonyms Synonym table (defaults to the loaded `taxon_synonyms`).
#' @return Markdown string, ending in a blank line, or "".
#' @keywords internal
#' @noRd
format_taxon_synonyms <- function(taxon, synonyms = taxon_synonyms) {
  if (is.null(synonyms)) return("")
  version <- attr(synonyms, "apc_version")
  source <- if (is.null(version) || is.na(version)) "APC" else sprintf("APC %s", version)
  rows <- synonyms[synonyms$taxon_name == taxon, , drop = FALSE]

  section <- function(kind, label) {
    x <- rows[rows$kind == kind, , drop = FALSE]
    if (nrow(x) == 0) return(NULL)
    items <- sprintf("- %s (%s)", x$scientific_name, x$taxonomic_status)
    # HTML, not markdown, inside <summary>: it is a raw HTML block
    sprintf("<details> <summary> <strong>%s</strong>: %d (%s) </summary>\n\n%s\n\n</details>\n\n",
            label, nrow(x), source, paste(items, collapse = "\n"))
  }

  out <- paste(c(
    section("synonym", "Synonyms"),
    section("misapplied", "Misapplied names")
  ), collapse = "")
  if (!nzchar(out)) out <- sprintf("**Synonyms**: none listed in the %s\n\n", source)
  out
}

#' Name under which a taxon's species means are published
#'
#' The published means roll subspecies, varieties and forms up to their
#' species; strip the infraspecific part of the name. A hybrid formula
#' (" x ") is kept whole: its parents are not its species.
#' @param taxon Taxon name.
#' @noRd
species_means_name <- function(taxon) {
  ifelse(grepl(" x ", taxon, fixed = TRUE), taxon, sub(" (subsp\\.|var\\.|f\\.) .*$", "", taxon))
}

#' Format numbers for display: 3 significant figures, whole numbers from 100 up
#'
#' The same rule as the data table (`mod_data_table.R`) and the browser portal,
#' so a mean reads the same everywhere. Fixed decimals would show small means,
#' such as seed masses, as 0.
#' @param x Numeric vector.
#' @noRd
format_sig3 <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("")
    if (abs(v) >= 100) return(sprintf("%.0f", v))
    format(signif(v, 3), digits = 3, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  }, character(1), USE.NAMES = FALSE)
}
