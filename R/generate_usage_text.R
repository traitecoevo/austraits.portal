
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
  metatdata <- jsonlite::read_json("inst/extdata/austraits/austraits.json")

  version <- metatdata$hits$hits[[1]]$metadata$version
  doi <- metatdata$hits$hits[[1]]$metadata$doi
  date <- metatdata$hits$hits[[1]]$metadata$publication_date
  concept_doi <- metatdata$hits$hits[[1]]$conceptdoi

  references <- data$source_primary_citation |> unique()
  keys <- data$source_primary_key |> unique()

  usage_text <- 
    sprintf(
"Trait data is sourced from AusTraits %s (Falster et al. 2021, %s), drawing from contributions from the following datasets: %s. Taxa were aligned against the Australian Plant Census (APC, <https://biodiversity.org.au/nsl/services/search/taxonomy>) using the R package {APCalign} (Wenk et al 2024a), by first, searching for alignments with known names (via fuzzy matching), and then using known alignments to update taxon names to the latest. Original taxon names attributed by the data collectors are included. Trait names were harmonised against the AusTraits Plant Dictionary (APD) (Wenk et al 2024b). Note that trait values were scored at different levels (individual, population or species), according to source; individual and population level values might not be representative of the trait values displayed by a species in other parts of its range. The full dataset is available at [doi: %s](http://doi.org/%s) and is made available under the CC BY 4.0 license (<https://creativecommons.org/licenses/by/4.0/>). The data is provided 'as is' without any warranties or guarantees of any kind.
  
References

- Falster et al %s. AusTraits %s [Data set]. Zenodo. doi: [%s](https://doi.org/%s)
- Falster et al 2021. AusTraits, a curated plant trait database for the Australian flora. Scientific Data 8, 254. doi: [10.1038/s41597-021-01006-6](http://doi.org/10.1038/s41597-021-01006-6)
- Wenk EH et al. (2024a) APCalign: an R package workflow and app for aligning and updating flora names to the Australian Plant Census. Australian Journal of Botany 72 BT24014. doi: [10.1071/BT24014](http://doi.org/10.1071/BT24014)
- Wenk EH et al. (2024b) The AusTraits plant dictionary. Scientific Data 11: 537. doi: [10.1038/s41597-024-03368-z](http://doi.org/10.1038/s41597-024-03368-z)
%s",
    version,
    date |> stringr::str_sub(1,4),
    keys |> paste(collapse = ", " ),
    concept_doi, concept_doi,
    version,
    date |> stringr::str_sub(1,4),
    doi, doi,
    paste("- ", references) |> paste(collapse = "\n" )
  )

  usage_text
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
