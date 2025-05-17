# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo,
## For now will use the R package and store in Github Releases see branch data-load
## Use austraits R package load_austraits() function to download data to the file path below
## Then create this parquet following code in data-raw/create-flat-austraits.R

# Load the austraits dataset
austraits <- arrow::open_dataset("inst/extdata/austraits/austraits-lite-obs.parquet")
# austraits <- arrow::open_dataset("inst/extdata/austraits/austraits-6.0.0-flatten.parquet")

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
# TODO: Not yet implemented.
### Coordinates - circle/bbox around coordinates?

### States by location properties


### APC distribution - May need APCalign::create_species_state_origin_matrix()
all_states_territories <- austraits  |> 
  extract_distinct_values(taxon_distribution) |> 
  paste(collapse = ", ")  |> 
  stringr::str_split(",")  |> 
  purrr::map(~trimws(.x))  |> 
  purrr::list_c() |>
  unique()  |> 
  stringr::word(1)  |> 
  unique()  |> 
  sort()

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

# Custom Github hyperlink icon
target <- bsplus::shiny_iconlink(name = "github")
target$attribs$href <- "https://github.com/traitecoevo/austraits.portal"
