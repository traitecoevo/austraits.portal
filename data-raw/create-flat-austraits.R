library(austraits)
library(arrow)
library(dplyr)

#------------------------------------------------------------
# Save lite version as a parquet
create_lite_austraits <- function() {
  # Create downloadable version
  austraits:::austraits_5.0.0_lite |> 
    flatten_database() |>
    mutate(row_id = row_number()) |>
    write_parquet("inst/extdata/austraits/austraits-lite-obs.parquet")

  # Create formatted version
  austraits:::austraits_5.0.0_lite |> 
    flatten_database() |>
    mutate(row_id = row_number()) |>
    format_database_for_display() |> 
    format_hyperlinks_for_display() |>
    write_parquet("inst/extdata/austraits/austraits-lite-display-obs.parquet")
}

#------------------------------------------------------------
# Save the full database as a parquet
## TODO: One day parquet of flattened database may be uploaded to Zenodo, needs row_id added to Zenodo upload
## For now will use the R package and store in Github Releases
create_full_austraits <- function() {
  # Load full database
  austraits <- load_austraits(version = "6.0.0", path = "inst/extdata/austraits", update = FALSE)
  austraits$definitions |> yaml::write_yaml("inst//extdata/austraits/definitions.yml")
  austraits$sources |> RefManageR::WriteBib("inst/extdata/austraits/sources.bib")

  # Flatten the database
  austraits_flat <- austraits |> 
    flatten_database() |> 
    mutate(row_id = row_number()) |> 
    mutate(measurement_remarks = iconv(measurement_remarks,
      from = "",      # Let R guess the current encoding
      to = "UTF-8",   # Convert to UTF-8
      sub = ""        # Remove any bytes that can't be converted
    ))

  print("Formatting for use")
  austraits_display <- austraits_flat

  # Database for DT display
  print("Formatting for display")
  austraits_display <- austraits_display |> 
    format_database_for_display() |> 
    format_hyperlinks_for_display()
  print("done")

  ## Save the flattened and display database
  write_parquet(austraits_flat, "inst/extdata/austraits/austraits-6.0.0-flatten.parquet")
  write_parquet(austraits_display, "inst/extdata/austraits/austraits-6.0.0-flatten-display.parquet")
}
#------------------------------------------------------------
# Save an intermediate sized version, to reduce size of the database
# Only has a subset of taxa and only core traits
create_mid_austraits <- function() {
  # Only core traits
  traits <- read_csv_arrow("inst/extdata/austraits/trait_groups_for_portal.csv") |> 
    filter(!is.na(core_trait)) |> 
    pull(trait)

  # Choose random selectioon of taxa, plus those in lite version
  set.seed(123)
  taxon_names <- c(
      flatten_austraits$taxon_name |> unique() |> sample(5000),
      austraits:::austraits_5.0.0_lite$traits$taxon_name |> unique()
      ) |> sort()

  # Filter and save
  flatten_mid <- 
      flatten_austraits |>
      filter(taxon_name %in% taxon_names, trait_name %in% traits) |>
      mutate(row_id = row_number())
  write_parquet(flatten_mid, "inst/extdata/austraits/austraits-6.0.0-mid-flatten.parquet")

  # Species averages
  flatten_mid_avg <- 
      flatten_mid |> estimate_species_trait_means()

  write_parquet(flatten_mid_avg, "inst/extdata/austraits/austraits-6.0.0-mid-flatten-means.parquet")
}

create_lite_austraits()