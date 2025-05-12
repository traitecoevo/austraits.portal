library(austraits)
library(arrow)

# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo, 
## For now will use the R package and store in Github Releases
austraits <- load_austraits(version = "6.0.0")

# Wrangle data
## Flatten the database
flatten_austraits <- austraits |> flatten_database()

## Save this as a parquet
write_parquet(flatten_austraits, "data/austraits/austraits-6.0.0-flatten.parquet")

## Save lite version as a parquet
write_parquet(austraits:::austraits_5.0.0_lite |> flatten_database(), "data/austraits/austraits-lite.parquet")

## Create species means

austraits <- arrow::read_parquet("inst/extdata/austraits/austraits-lite.parquet")

# Numeric traits
traits <- c("leaf_area", "leaf_mass_per_area", "leaf_nitrogen_content", "leaf_phosphorus_content", "leaf_carbon_content", "leaf_silicon_content", "leaf_stable_isotope_ratio")

x <- austraits_weighted_means(austraits, traits)

# Categorical traits
traits <- c("serotiny", "dispersal_syndrome", "clonal_spread_mechanism", "bud_bank_location")


categorical_trait_summary(austraits, traits)

categorical_trait_value_summary(austraits, traits)
