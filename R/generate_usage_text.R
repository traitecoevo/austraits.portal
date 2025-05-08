
generate_usage_and_citations_text <- function(version_tag, citations, references) {

  usage_text <- sprintf(
"This data is sourced from AusTraits v%s (Falster et al. 2021, Falster et al. 2024), drawing from contributions from the following datasets: %s. Taxa were aligned against the Australian Plant Census (APC, https://biodiversity.org.au/nsl/services/search/taxonomy) using the R package {APCalign} (Wenk et al 2024a), by first, searching for alignments with known names (via fuzzy matching), and then using known alignments to update taxon names to the latest. Original taxon names attributed by the data collectors are included. Trait names were harmonised against the AusTraits Plant Dictionary (APD) (Wenk et al 2024b). Note that trait values were scored at different levels (individual, population or species), according to source; individual and population level values might not be representative of the trait values displayed by a species in other parts of its range. The full dataset is available at https://zenodo.org/records/11188867 and is made available under the CC BY 4.0 license (https://creativecommons.org/licenses/by/4.0/). The data is provided 'as is' without any warranties or guarantees of any kind.
  
References

- Contributors to AusTraits (2024). AusTraits v6.0.0 [Data set]. Zenodo. https://doi.org/10.5281/zenodo.11188867
- Falster et al 2021. AusTraits, a curated plant trait database for the Australian ora.
Scientific Data 8, 254. doi:10.1038/s41597-021-01006-6
- Wenk EH et al. (2024) APCalign: an R package workflow and app for aligning and updating flora names to the Australian Plant Census. Australian Journal of Botany 72. DOI: http://doi.org/10.1071/BT24014
- Wenk EH et al. (2024) The AusTraits plant dictionary. Scientific Data 11: 537. DOI: 10.1038/s41597-024-03368-z
%s",
    "austraits_version", 
    "citations",
    "references"
  )

  usage_text
}
