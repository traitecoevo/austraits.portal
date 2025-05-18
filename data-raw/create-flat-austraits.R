library(austraits)
library(arrow)
library(dplyr)

#------------------------------------------------------------
# Save lite version as a parquet
arrow::write_parquet(austraits:::austraits_5.0.0_lite |> flatten_database(), "inst/extdata/austraits/austraits-lite.parquet")
RefManageR::WriteBib(austraits:::austraits_5.0.0_lite$sources, "inst/extdata/austraits/sources.bib")

#------------------------------------------------------------
# Save the full database as a parquet

## Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo, needs row_id added to Zenodo upload
## For now will use the R package and store in Github Releases
austraits <- load_austraits(version = "6.0.0", path = "inst/extdata/austraits", update = FALSE)

## Flatten the database
flatten_austraits <- austraits |> flatten_database()  |> 
    dplyr::mutate(row_id = dplyr::row_number())  |> 
    dplyr::mutate(measurement_remarks = iconv(measurement_remarks,
                    from = "",      # Let R guess the current encoding
                    to = "UTF-8",   # Convert to UTF-8
                    sub = ""        # Remove any bytes that can't be converted
    ))

## Save the flattened database
arrow::write_parquet(flatten_austraits, "inst/extdata/austraits/austraits-6.0.0-flatten.parquet")
RefManageR::WriteBib(austraits$sources, "inst/extdata/austraits/sources.bib")

#------------------------------------------------------------
# Save an intermediate sized version, to reduce size of the database
# Only has a subset of taxa and only core traits

## Only core traits
traits <- read_csv_arrow("inst/extdata/austraits/trait_groups_for_portal.csv") |> filter(!is.na(core_trait)) |> pull(trait)

## Choose random selectioon of taxa, plus those in lite version
set.seed(123)
taxon_names <- c(
    flatten_austraits$taxon_name |> unique() |> sample(5000),
    austraits:::austraits_5.0.0_lite$traits$taxon_name |> unique()
    ) |> sort()

## filter and save
flatten_mid <- 
    flatten_austraits |>
    dplyr::filter(taxon_name %in% taxon_names, trait_name %in% traits) |>
    dplyr::mutate(row_id = dplyr::row_number())
arrow::write_parquet(flatten_mid, "inst/extdata/austraits/austraits-6.0.0-mid-flatten.parquet")

## species averages
flatten_mid_avg <- 
    flatten_mid |> estimate_species_trait_means()

arrow::write_parquet(flatten_mid_avg, "inst/extdata/austraits/austraits-6.0.0-mid-flatten-means.parquet")

