options(shiny.launch.browser = TRUE)

# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo,
## For now will use the R package and store in Github Releases see branch data-load
## Use austraits R package load_austraits() function to download data to the file path below
## Then create this parquet following code in data-raw/create-flat-austraits.R

# Custom logic
`%not_in%` <- Negate(`%in%`)

# set the path to the data
#data_path <- "inst/extdata/austraits/austraits-5.0.0-lite"
data_path <- "inst/extdata/austraits/austraits-6.0.0-mid"
#data_path <- "inst/extdata/austraits/austraits-6.0.0-full"

# Load the datasets
austraits <- arrow::open_dataset(file.path(data_path, "austraits-data.parquet"))
austraits_display <- arrow::open_dataset(file.path(data_path, "austraits-display.parquet"))
trait_definitions <- yaml::read_yaml(file.path(data_path, "definitions.yml"))
trait_groups <- readr::read_csv(
  "inst/extdata/austraits/trait_groups_for_portal.csv",
  col_types = readr::cols(.default = readr::col_character())
  )
metatdata <- jsonlite::read_json("inst/extdata/austraits/austraits.json")

columns_display <- c(
  "dataset_id", "taxon_name", "genus", "family", "trait_name", "value", "unit",
  "entity_type", "value_type", "basis_of_value", "replicates", "basis_of_record",
  "life_stage", "collection_date", "measurement_remarks", "original_name", 
  "location_name", "latitude (deg)", "longitude (deg)", "location_properties",
  "treatment_context_properties", "plot_context_properties", 
  "entity_context_properties", "temporal_context_properties", 
  "method_context_properties", "source_primary_citation", "data_contributors",
  "taxon_rank", "taxon_distribution", "establishment_means"
)

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
