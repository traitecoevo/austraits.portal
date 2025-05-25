library(austraits)
library(arrow)
library(dplyr)

#TODO: Use a consistent naming convention, the 'obs' doesn't add much info about what the parquet is
# My suggestion is to use 'austraits-lite.parquet' 

# LITE VERSION
#------------------------------------------------------------
# Save lite version as a parquet
austraits:::austraits_5.0.0_lite |> 
    austraits::flatten_database() |>
    dplyr::mutate(row_id = dplyr::row_number())|>
    arrow::write_parquet("inst/extdata/austraits/austraits-lite-obs.parquet")  

# Save the display version of the lite database

austraits:::austraits_5.0.0_lite |> 
    austraits::flatten_database() |>
    dplyr::mutate(row_id = dplyr::row_number())|>
    format_database_for_display() |> 
    format_hyperlinks_for_display() |> 
    arrow::write_parquet("inst/extdata/austraits/austraits-lite-display.parquet")
#------------------------------------------------------------

# FULL VERSION
# Save the full database as a parquet

## Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo, needs row_id added to Zenodo upload
## For now will use the R package and store in Github Releases
austraits <- load_austraits(version = "6.0.0", path = "inst/extdata/austraits", update = FALSE)
austraits$definitions |> yaml::write_yaml("inst//extdata/austraits/definitions.yml")
austraits$sources |> RefManageR::WriteBib("inst/extdata/austraits/sources.bib")

## Flatten the database
flatten_austraits <- austraits |> flatten_database()  |> 
    mutate(row_id = row_number())  |> 
    mutate(measurement_remarks = iconv(measurement_remarks,
                    from = "",      # Let R guess the current encoding
                    to = "UTF-8",   # Convert to UTF-8
                    sub = ""        # Remove any bytes that can't be converted
    ))

## Save the flattened database
write_parquet(flatten_austraits, "inst/extdata/austraits/austraits-6.0.0-flatten.parquet")

#------------------------------------------------------------
# Save an intermediate sized version, to reduce size of the database
# Only has a subset of taxa and only core traits

## Only core traits
traits <- arrow::read_csv_arrow("inst/extdata/austraits/trait_groups_for_portal.csv") |> filter(!is.na(core_trait)) |> pull(trait)

## Choose random selectioon of taxa, plus those in lite version
set.seed(123)
taxon_names <- c(
    flatten_austraits$taxon_name |> unique() |> sample(5000),
    austraits:::austraits_5.0.0_lite$traits$taxon_name |> unique()
    ) |> sort()

## Filter taxon and traits and save as parquet
flatten_mid <- 
    flatten_austraits |>
    filter(taxon_name %in% taxon_names, trait_name %in% traits) |>
    mutate(row_id = row_number())
write_parquet(flatten_mid, "inst/extdata/austraits/austraits-6.0.0-mid-flatten.parquet")

## Estimate species averages
flatten_mid_avg <- 
    flatten_mid |> estimate_species_trait_means()

write_parquet(flatten_mid_avg, "inst/extdata/austraits/austraits-6.0.0-mid-flatten-means.parquet")

