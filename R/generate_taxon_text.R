

generate_taxon_text <- function(data, taxon) {

#  data <- austraits 

  data_taxon <- data |>
    dplyr::filter(taxon_name == taxon)

  taxon_info <- data_taxon |>
    dplyr::select(taxon_name, taxon_distribution, taxon_rank:aligned_name_taxonomic_status) |>
    dplyr::slice(1) |> as.list()

  portal_links <- generate_taxon_portal_links(taxon_info)

  taxon_description <- 
    sprintf(

"## AusTraits Trait Porfile for *%s*

FAMILY %s

Taxonomy: %s

**Distribution**: %s

**Profiles**:

%s

## Traits 

AusTraits contains %s traits for *%s* from %s observations in %s datasets.

Top source datasets: %s. See below for a full list of sources.

Download: [full data (csv)](%s), [summary table (csv)](%s)



%s

##
"
    , 
    taxon_name,
    taxon_info$family, 
    taxon_info$scientific_name, 
    taxon_info$taxon_distribution,
    sprintf("- %s: <%s>", portal_links$source, portal_links$url) |> paste(collapse = "\n"),
    3, 4, 5, "ABRS_1981", "link", "link", "Species mean traits", "- ref1\n- ref2\n- ref3\n"
    )

  taxon_description
}


# ▼ General habit

# Trait 1: value (y observations in z datasets) ?sources. Link to APD (add hover text if easy)
# Trait 2: value (y observations in z datasets)

# Core traits with no data: fire response, growth, form,...

# ▶ Leaves

# Meaningful ordering

# ▶ Flowers


# ▶ Fruits and seeds


# ▶ Stems

generate_taxon_portal_links <- function(taxon_info) {
  # Generate links to other portals
  dplyr::tribble(
    ~source, ~url,
    "APC", taxon_info$taxon_id,
    "NSW Flora", sprintf("https://plantnet.rbgsyd.nsw.gov.au/cgi-bin/NSWfl.pl?page=nswfl&lvl=sp&name=%s", gsub(" ", "~", taxon_info$taxon_name)),
    "Vic Flora", "https://vicflora.rbg.vic.gov.au/flora/taxon/",
    "Flora of Australia", sprintf("https://profiles.ala.org.au/opus/foa/profile/%s", gsub(" ", "%20", taxon_info$taxon_name)),
    "ALA", sprintf("https://bie.ala.org.au/species/%s", taxon_info$taxon_id),
    "iNaturalist", sprintf("https://www.inaturalist.org/taxa/search?q=%s", gsub(" ", "-", taxon_info$taxon_name))
  )
}

#' Export BibTeX Entries for Data
#'
#' Exports BibTeX entries corresponding to the provided keys to a specified file.
#'
#' @param keys A character vector of BibTeX entry keys to export.
#' @param filename A string specifying the path to the output file where the BibTeX entries will be saved.
#' @param refs A BibTeX object containing the references. This is typically read from a file.
#' @return Invisibly returns \code{NULL}. The function is called for its side effect of writing to a file.
#'
#' @examples
#' \dontrun{
#' export_bibtex_for_data(c("key1", "key2"), "output.bib")
#' }
#'
#' @export
export_bibtex_for_data <- function(keys, filename, 
    refs = 
        RefManageR::ReadBib(
        file = "inst/extdata/austraits/sources.bib",
        check = FALSE, .Encoding = "UTF-8")
) {
  # Get the bibtex for the keys
  refs <- refs[keys]

  # Write the bibtex to a file
  RefManageR::WriteBib(
    refs,
    file = filename,
    .Encoding = "UTF-8",
    check = FALSE
  )
}
