# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo,
## For now will use the R package and store in Github Releases see data-raw/create-flat-austraits.R
austraits <-
  arrow::open_dataset("inst/extdata/austraits/austraits-6.0.0-flatten.parquet") 

# Set up possible values for selectize menus

## Taxonomy
### Unique values of family
all_family <- austraits |> 
  extract_distinct_values(family)

### Unique values of genus
all_genus <- austraits |> 
  extract_distinct_values(genus)

## Unique values of taxon_name
all_taxon_names <- austraits |>
extract_distinct_values(taxon_name)

## Location
# TODO: Not yet implemented. Need APCalign? 
# all_locations <- austraits |>
#   extract_distinct_values(taxon_distribution)

## Traits
### Unique values of taxon_name
all_traits <- austraits |>
  extract_distinct_values(trait_name)

## Other sidebar values
### Unique values of BoR
all_bor <- austraits |>
  extract_distinct_values(basis_of_record)

## Unique values of age/lifestage
all_age <- austraits |>
  extract_distinct_values(life_stage)
