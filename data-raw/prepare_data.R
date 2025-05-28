library(austraits)
library(arrow)
library(dplyr)
source("R/a-helper.R")

# Prepare the data for the portal

# Lite version
austraits:::austraits_5.0.0_lite |> 
  prepare_data_for_portal("inst/extdata/austraits/austraits-5.0.0-lite")

# Full version
austraits_6.0.0 <- 
  austraits::load_austraits(version = "6.0.0", path = "inst/extdata/austraits", update = FALSE)

austraits_6.0.0 |> 
  prepare_data_for_portal("inst/extdata/austraits/austraits-6.0.0-full")

# Mid version

# Create an intermediate sized version, to reduce size of the database
# Only has a subset of taxa and only core traits
## Only core traits
traits <- arrow::read_csv_arrow("inst/extdata/austraits/trait_groups_for_portal.csv") |> dplyr::filter(!is.na(core_trait)) |> dplyr::pull(trait)
## Choose random selectioon of taxa, plus those in lite version
set.seed(123)
taxon_names <- c(
    austraits_6.0.0$traits$taxon_name |> unique() |> sample(5000),
    austraits:::austraits_5.0.0_lite$traits$taxon_name |> unique()
    ) |> sort()
## Filter taxon and traits and save as parquet
austraits_6.0.0_mid <- austraits_6.0.0
austraits_6.0.0_mid$traits <-
  austraits_6.0.0_mid$traits |> dplyr::filter(trait_name %in% traits, taxon_name %in% taxon_names)

austraits_6.0.0_mid |> prepare_data_for_portal("inst/extdata/austraits/austraits-6.0.0-mid")
