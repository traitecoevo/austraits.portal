
#' Generate Usage and Citations Text
#'
#' Generates formatted text for usage instructions and citation information based on the selected data
#'
#' @param data A data object containing from which references are sourced, needed to generate usage and citation text.
#'
#' @return A character string with formatted usage and citation details.
#'
#' @examples
#' \dontrun{
#'   usage_text <- generate_usage_and_citations_text(my_data)
#'   cat(usage_text)
#' }
#' @export

generate_usage_and_citations_text <- function(data) {

  # Extract the version of the package
  version <- metatdata$hits$hits[[1]]$metadata$version
  doi <- metatdata$hits$hits[[1]]$metadata$doi
  date <- metatdata$hits$hits[[1]]$metadata$publication_date
  concept_doi <- metatdata$hits$hits[[1]]$conceptdoi

  keys <- data$source_primary_key |> stringr::str_split(pattern = "; ") |> unlist() |> sort() |> unique()
  # then get the references. Taking these from the sources data frame loaded in data_loading.R instead of data frame, as species means have multiple sources pasted together (so complicated to parse)
  references <- sources |> dplyr::filter(source_primary_key %in% keys) |> dplyr::pull(source_primary_citation)

  usage_text <- 
    sprintf(
"The AusTraits dataset is available at [doi: %s](http://doi.org/%s). AusTraits data is distributed under the CC BY 4.0 license (<https://creativecommons.org/licenses/by/4.0/>). The data is provided 'as is' without any warranties or guarantees of any kind. The AusTraits database is described in Falster et al. 2021, drawing from many the contributed datasets. Publications using AusTraits data should cite the AusTraits data paper and relevant datasets.

Taxa were aligned against the Australian Plant Census (APC, <https://biodiversity.org.au/nsl/services/search/taxonomy>) using the R package {APCalign} (Wenk et al 2024a), by first, searching for alignments with known names (via exact, then fuzzy matching), and then using known alignments to update taxon names to the currently accepted name. Original taxon names attributed by the data collectors are included. 

Trait names were harmonised against the AusTraits Plant Dictionary (APD) (Wenk et al 2024b). 

Note that trait values were scored at different levels (individual, population or species), according to source; individual and population level values might not be representative of the trait values displayed by a species in other parts of its range. The full dataset is available at [doi: %s](http://doi.org/%s)

The following datasets contributed data included in the selected search: %s
  
**General AusTraits references**

- Falster et al %s. AusTraits %s [Data set]. Zenodo. doi: [%s](https://doi.org/%s)
- Falster et al 2021. AusTraits, a curated plant trait database for the Australian flora. Scientific Data 8, 254. doi: [10.1038/s41597-021-01006-6](http://doi.org/10.1038/s41597-021-01006-6)
- Wenk EH et al. (2024a) APCalign: an R package workflow and app for aligning and updating flora names to the Australian Plant Census. Australian Journal of Botany 72 BT24014. doi: [10.1071/BT24014](http://doi.org/10.1071/BT24014)
- Wenk EH et al. (2024b) The AusTraits plant dictionary. Scientific Data 11: 537. doi: [10.1038/s41597-024-03368-z](http://doi.org/10.1038/s41597-024-03368-z)

**Primary dataset sources**\n
(subset, for large datasets full list is provided in the download)

%s",
    concept_doi, concept_doi,    # First paragraph DOI links
    concept_doi, concept_doi,    # "full dataset" DOI links
    keys |> paste(collapse = ", "),  # Dataset list
    date |> stringr::str_sub(1,4),   # Year for Falster et al
    version,                          # Version number
    doi, doi,                         # Zenodo DOI links
    paste("- ", references) |> paste(collapse = "\n")  # References list
  )

  usage_text |> commonmark::markdown_html() |> add_target_blank() |> HTML()
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
export_bibtex_for_data <- function(keys, filename, refs = get_sources_bib()) {
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
