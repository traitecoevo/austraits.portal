
#' Generate Text Summary for a trait
#'
#' Creates a textual summary for a specified trait based on the provided data.
#'
#' @param data_trait A data frame
#' @param trait A character string specifying the name of the trait to summarize.
#'
#' @return A character string containing the generated summary for the specified trait.
#'
#' @examples
#' \dontrun{
#' data_trait <- arrow::open_dataset("inst/extdata/austraits/austraits-6.0.0-mid-flatten.parquet") |> filter(trait_name == "leaf_area") |> collect()
#' generate_trait_profile(data_trait, "leaf_area")
#' }
generate_trait_profile <- function(data_trait) {

  # Check validity
  if (nrow(data_trait) == 0 || dplyr::n_distinct(data_trait$trait_name) > 1) {
    output <- list()
    output[[1]] <- "Please select a single trait to view."
    for(i in 2:3) {
      output[[i]] <- ""
    }
    output[[4]] <- leaflet::leaflet()
    return(output)
  }

  trait <- data_trait$trait_name[1]

  trait_definition <- yaml::read_yaml("inst/extdata/austraits/definitions.yml")[[trait]]

  data_geo <-
    data_trait |>
    filter(!is.na(`latitude (deg)`), !is.na(`longitude (deg)`)) |>
    mutate(
      `latitude (deg)` = as.numeric(`latitude (deg)`),
      `longitude (deg)` = as.numeric(`longitude (deg)`)
    )
  
  ## Generate output, as a list of strings & plots
  output <- list()
  output[[1]] <- 
  sprintf(
"## %s 

- **Label**: %s
- **Entity URI**: [%s](%s),
- **Type**: %s
- **Definition:** %s
- **Additional comments**: %s
%s

See the AusTraits Plant Dictionary (<%s>) for additional information about this trait.

### Coverage

For this trait, AusTraits includes a total of **%s** records. This includes:

- data for **%s** species.
- data for **%s** families.
- data from **%s** datasets (see below for a list of sources)
  ", 
  trait, 
  trait_definition$label,
  trait_definition$entity_URI,
  trait_definition$entity_URI,
  trait_definition$type,
  trait_definition$description,# |> stringr::str_replace("^.*\\;", ""),
  ifelse(!is.null(trait_definition$comments), trait_definition$comments, "None"),
  ifelse(trait_definition$type == "numeric", 
      sprintf("- **units**: %s\n- **allowable range**: %s -  %s %s\n", 
        trait_definition$units,
        trait_definition$allowed_values_min, 
        trait_definition$allowed_values_max,
        trait_definition$units),
      sprintf("- **Allowed values**: \n\n %s",
        trait_definition$allowed_values_levels |>
        convert_list_to_df1() |>
        rename(description = value, value = key) |>
        knitr::kable() |>
        kableExtra::kable_styling(full_width = TRUE,
                                 bootstrap_options = c("striped", "hover", "condensed"))
      )
  ),
  trait_definition$entity_URI,
  data_trait |> nrow(),
  data_trait$taxon_name |> dplyr::n_distinct(),
  data_trait$family |> dplyr::n_distinct(),
  data_trait$dataset_id |> dplyr::n_distinct()
  ) |> commonmark::markdown_html() |> HTML()

  # Geo text
    output[[3]] <-
    sprintf(
"
Of the %s records for this trait, **%s** have latitude and longitude coordinates. 

 The map below shows the geographical distribution of trait data for this trait. The data are shown as points on the map, with the size of the point indicating the number of records at that location. 

", 
  data_trait |> nrow(),
  nrow(data_geo)
  ) |> commonmark::markdown_html() |> HTML()

  # Geomap
      output[[4]] <-
      leaflet::leaflet(data = data_geo) |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(
        lng = ~`longitude (deg)`,
        lat = ~`latitude (deg)`,
        label = ~as.character(dataset_id),
        radius = 4,
        fillOpacity = 0.7
      )


  output
}

convert_list_to_df1 <- function(my_list) {
  for (f in names(my_list)) {
    if (is.null(my_list[[f]])) {
      my_list[[f]] <- NA
    }
  }
  tibble::tibble(key = names(my_list), value = unname(unlist(my_list)))
}
