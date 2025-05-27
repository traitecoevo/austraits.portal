library(austraits)
library(arrow)
library(dplyr)
source("R/a-helper.R")

# TODO: Filing structure for display/species averages?
# TODO: Encoding issue in full database

# LITE VERSION
#------------------------------------------------------------
# Save lite version as a parquet
austraits_lite_flatten <- austraits:::austraits_5.0.0_lite |> 
    austraits::flatten_database() |>
    dplyr::mutate(row_id = dplyr::row_number())

austraits_lite_flatten |> 
    arrow::write_parquet("inst/extdata/austraits/austraits-lite-flatten.parquet")  

# Save the display version of the lite database
austraits_lite_flatten |> 
    format_database_for_display() |> 
    format_hyperlinks_for_display() |> 
    arrow::write_parquet("inst/extdata/austraits/austraits-lite-display.parquet")
#------------------------------------------------------------
# FULL VERSION
#------------------------------------------------------------
# Save the full database as a parquet

## Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo, needs row_id added to Zenodo upload
## For now will use the R package and store in Github Releases
austraits <- austraits::load_austraits(version = "6.0.0", path = "inst/extdata/austraits", update = FALSE)

# Saving definitions and sources for use in the portal
austraits$definitions |> yaml::write_yaml("inst//extdata/austraits/definitions.yml")
austraits$sources |> RefManageR::WriteBib("inst/extdata/austraits/sources.bib")

## Flatten the database
austraits_full_flatten <- 
    austraits |> 
    austraits::flatten_database()  |> 
    dplyr::mutate(row_id = dplyr::row_number())  |> 
    dplyr::mutate(measurement_remarks = iconv(measurement_remarks,
                    from = "",      # Let's R guess the current encoding
                    to = "UTF-8",   # Convert to UTF-8
                    sub = ""        # Remove any bytes that can't be converted
    ))

## Save the flattened database
arrow::write_parquet(austraits_full_flatten, "inst/extdata/austraits/austraits-6.0.0-flatten.parquet")

# Save the display version of the full  database
austraits_full_flatten  |> 
    format_database_for_display() |> 
    format_hyperlinks_for_display() |> 
    arrow::write_parquet("inst/extdata/austraits/austraits-6.0.0-display.parquet")
#------------------------------------------------------------
# MID VERSION
#------------------------------------------------------------
# Save an intermediate sized version, to reduce size of the database
# Only has a subset of taxa and only core traits

## Only core traits
traits <- arrow::read_csv_arrow("inst/extdata/austraits/trait_groups_for_portal.csv") |> filter(!is.na(core_trait)) |> pull(trait)

## Choose random selectioon of taxa, plus those in lite version
set.seed(123)
taxon_names <- c(
    austraits_full_flatten$taxon_name |> unique() |> sample(5000),
    austraits:::austraits_5.0.0_lite$traits$taxon_name |> unique()
    ) |> sort()

## Filter taxon and traits and save as parquet
flatten_mid <- 
    austraits_full_flatten |>
    dplyr::filter(taxon_name %in% taxon_names, trait_name %in% traits) |>
    dplyr::mutate(row_id = row_number())

arrow::write_parquet(flatten_mid, "inst/extdata/austraits/austraits-6.0.0-mid-flatten.parquet")

# Save the display version of the mid database
flatten_mid  |> 
    format_database_for_display() |> 
    format_hyperlinks_for_display() |> 
    arrow::write_parquet("inst/extdata/austraits/austraits-6.0.0-mid-display.parquet")

## Estimate species averages
flatten_mid_avg <- 
    flatten_mid |> 
    estimate_species_trait_means()

arrow::write_parquet(flatten_mid_avg, "inst/extdata/austraits/austraits-6.0.0-mid-flatten-means.parquet")

