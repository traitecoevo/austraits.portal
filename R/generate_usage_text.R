
generate_usage_and_citations_text <- function(data) {

  # Extract the version of the package
  metatdata <- jsonlite::read_json("inst/extdata/austraits/austraits.json")

  version <- metatdata$hits$hits[[1]]$metadata$version
  doi <- metatdata$hits$hits[[1]]$metadata$doi
  date <- metatdata$hits$hits[[1]]$metadata$publication_date
  concpet_doi <- metatdata$hits$hits[[1]]$conceptdoi

  references <- data$source_primary_citation |> unique()
  keys <- data$source_primary_key |> unique()

  usage_text <- 
    sprintf(
"Trait data is sourced from AusTraits %s (Falster et al. 2021, %s), drawing from contributions from the following datasets: %s. Taxa were aligned against the Australian Plant Census (APC, https://biodiversity.org.au/nsl/services/search/taxonomy) using the R package {APCalign} (Wenk et al 2024a), by first, searching for alignments with known names (via fuzzy matching), and then using known alignments to update taxon names to the latest. Original taxon names attributed by the data collectors are included. Trait names were harmonised against the AusTraits Plant Dictionary (APD) (Wenk et al 2024b). Note that trait values were scored at different levels (individual, population or species), according to source; individual and population level values might not be representative of the trait values displayed by a species in other parts of its range. The full dataset is available at %s and is made available under the CC BY 4.0 license (https://creativecommons.org/licenses/by/4.0/). The data is provided 'as is' without any warranties or guarantees of any kind.
  
References

- Falster et al %s. AusTraits %s [Data set]. Zenodo. https://doi.org/%s
- Falster et al 2021. AusTraits, a curated plant trait database for the Australian flora. Scientific Data 8, 254. http://doi.org/10.1038/s41597-021-01006-6
- Wenk EH et al. (2024a) APCalign: an R package workflow and app for aligning and updating flora names to the Australian Plant Census. Australian Journal of Botany 72 BT24014. http://doi.org/10.1071/BT24014
- Wenk EH et al. (2024b) The AusTraits plant dictionary. Scientific Data 11: 537. http://doi.org/10.1038/s41597-024-03368-z
%s",
    version,
    date |> stringr::str_sub(1,4),
    keys |> paste(collapse = ", " ),
    concpet_doi,
     version,
    date |> stringr::str_sub(1,4),
    doi,
    paste("- ", references) |> paste(collapse = "\n" )
  )

  usage_text
}
