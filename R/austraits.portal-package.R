#' @keywords internal
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import bslib
#' @import arrow
#' @import dplyr
#' @import DT
#' @importFrom memoise memoise
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

utils::globalVariables(c(
	"Entity", "Group", "URLdecode", "all_replicates", "basis_of_record",
	"binomial", "colour", "core_trait", "counts", "counts_per_value",
	"data_available", "dataset_id", "dbDisconnect", "density",
	"establishment_means", "family", "family_order", "flora_replicates",
	"formatted", "genus", "head", "key", "keywords", "lat_num",
	"latitude", "life_stage", "location_id", "location_replicates",
	"log10_value", "lon_num", "longitude", "mean_value",
	"measurement_remarks", "median", "n_records", "na.omit",
	"observation_id", "original_name", "prop", "reference", "regex",
	"replicates", "req_body_json", "req_headers", "req_perform",
	"request", "resp_body_json", "row_counter", "row_id", "scaled_by_obs",
	"scientific_name", "scientific_name_id", "shapes",
	"source_id", "source_primary_citation", "source_primary_key", "species_per_family",
	"structure_measured", "taxon_distribution", "taxon_id",
	"taxon_id_family", "taxon_id_genus", "taxon_name",
	"taxon_name_alternatives", "taxon_rank", "taxonomic_dataset",
	"taxonomic_status", "text", "tmp_summary", "total", "total_counts",
	"total_per_obs", "trait", "trait_group_for_portal", "trait_name",
	"trinomial", "unit", "value", "value_count", "value_max",
	"value_mean", "value_median", "value_min", "value_type", "latitude (deg)",  "longitude (deg)"
))
