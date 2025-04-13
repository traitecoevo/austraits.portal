# Load data
## TODO: One day parquet of flattened database may be uploaded to Zenodo,
## For now will use the R package and store in Github Releases see data-raw/create-flat-austraits.R

load_parquet <- function(dir = system.file("extdata/austraits", package = "austraits.portal")) {
  # Check if the directory exists
  if (!dir.exists(dir)) {
    stop("Directory does not exist: ", dir)
  } 

  # Get latest version
 get_latest_version <- function(dir = system.file("extdata/austraits", package = "austraits.portal")){ 
  if(length(list.files(dir)) == 0){
    stop("No files found in the directory: ", dir)
  }

  grep("[0-9]+\\.[0-9]+\\.[0-9]+", list.files(dir), value = TRUE) |>
    gsub(pattern = "austraits-|\\-flatten.parquet", replacement = "")  |> 
    numeric_version() |>
    sort(decreasing = TRUE)  |> 
    dplyr::first()  |> 
    as.character()
  }

  # Load the first parquet file
  dataset <- arrow::open_dataset(paste0(dir, "/austraits-", get_latest_version(dir), "-flatten.parquet"))
          # arrow::open_dataset("inst/extdata/austraits/austraits-6.0.0-flatten.parquet") 
          # arrow::open_dataset("inst/extdata/austraits/austraits-lite.parquet") 
  
  return(dataset)
}

# Load the austraits dataset
austraits <- load_parquet()

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


### APC distribution - Need APCalign::create_species_state_origin_matrix()


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
