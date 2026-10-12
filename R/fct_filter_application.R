#' Escape regular-expression metacharacters
#'
#' @param x Character vector of literal strings.
#' @return `x` with every regex metacharacter backslash-escaped.
#' @keywords internal
#' @noRd
escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

#' Regex matching any of several literal values within a `; `-separated list
#'
#' Species-average rows hold several dataset IDs joined with `"; "`. The
#' pattern matches whole list elements only, so `Bryant_2021` does not also
#' match `Bryant_2021_2`.
#' @noRd
list_element_pattern <- function(values) {
  paste0("(^|; )(", paste(escape_regex(values), collapse = "|"), ")(;|$)")
}

#' Regex for a free-text search
#'
#' The text is matched literally, case-insensitively by the caller; `|`
#' separates alternatives (documented in the App Information tab).
#' @noRd
free_text_pattern <- function(text) {
  parts <- trimws(strsplit(tolower(text), "|", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return(NULL)
  paste(escape_regex(parts), collapse = "|")
}

#' Traits matching the selected trait features
#'
#' @param trait_groups Trait groups table.
#' @param parsed_trait The `trait` element of a parsed filter object.
#' @return Character vector of trait names, or NULL when no trait filter applies.
#' @keywords internal
#' @noRd
matching_trait_names <- function(trait_groups, parsed_trait) {
  if (is.null(parsed_trait$trait_name) && is.null(parsed_trait$trait_grouping) &&
      is.null(parsed_trait$structure_measured) && is.null(parsed_trait$keywords)) {
    return(NULL)
  }

  matching_traits <- trait_groups$trait

  if (!is.null(parsed_trait$trait_grouping)) {
    matching_traits <- trait_groups$trait[trait_groups$trait_group_for_portal %in% parsed_trait$trait_grouping]
  }

  # Structure and keyword fields hold lists of terms; match the selected terms literally
  contains_any <- function(field, terms) {
    Reduce(`|`, lapply(terms, function(term) grepl(term, field, fixed = TRUE)), FALSE)
  }

  if (!is.null(parsed_trait$structure_measured)) {
    matching_traits <- intersect(matching_traits,
      trait_groups$trait[contains_any(trait_groups$structure_measured, parsed_trait$structure_measured)])
  }

  if (!is.null(parsed_trait$keywords)) {
    matching_traits <- intersect(matching_traits,
      trait_groups$trait[contains_any(trait_groups$keywords, parsed_trait$keywords)])
  }

  if (!is.null(parsed_trait$trait_name)) {
    matching_traits <- intersect(matching_traits, parsed_trait$trait_name)
  }

  matching_traits
}

#' Apply all filters to dataset
#'
#' @description Applies categorical, location, and custom filters in one pass.
#'   Exact-match filters use `%in%`, which DuckDB evaluates as an `IN` list
#'   when the data is read with `read_parquet()` (see `load_portal_data()`).
#' @param data Base dataset (lazy DuckDB table)
#' @param parsed_filters Parsed filter object from parse_filters()
#' @return Filtered dataset (lazy query)
#' @keywords internal
#' @noRd
apply_filters <- function(data, parsed_filters) {

  filter_expressions <- list()
  add_filter <- function(e) filter_expressions[[length(filter_expressions) + 1]] <<- e

  # 1. TRAIT METADATA FILTERS

  matching_traits <- matching_trait_names(trait_groups, parsed_filters$trait)
  if (!is.null(matching_traits)) {
    # No matching trait means no rows, not "no filter"
    add_filter(expr(trait_name %in% !!matching_traits))
  }

  # 2. TAXON FILTERS

  if (!is.null(parsed_filters$taxon$family)) {
    add_filter(expr(family %in% !!parsed_filters$taxon$family))
  }

  if (!is.null(parsed_filters$taxon$genus)) {
    add_filter(expr(genus %in% !!parsed_filters$taxon$genus))
  }

  if (!is.null(parsed_filters$taxon$taxon_name)) {
    add_filter(expr(taxon_name %in% !!parsed_filters$taxon$taxon_name))
  }

  # 3. OTHER CATEGORICAL FILTERS

  if (!is.null(parsed_filters$other$basis_of_record)) {
    add_filter(expr(basis_of_record %in% !!parsed_filters$other$basis_of_record))
  }

  if (!is.null(parsed_filters$other$life_stage)) {
    add_filter(expr(life_stage %in% !!parsed_filters$other$life_stage))
  }

  # 4. LOCATION FILTERS

  if (!is.null(parsed_filters$location$location) && parsed_filters$location$location != "") {

    # Check if geo columns exist (species dataset doesn't have them!)
    has_geo_cols <- all(c("latitude (deg)", "longitude (deg)") %in% colnames(data))

    if (parsed_filters$location$location == "georeferenced" && has_geo_cols) {
      lat_col <- rlang::sym("latitude (deg)")
      lon_col <- rlang::sym("longitude (deg)")

      # Coordinates are numeric in the display view; non-numeric text is NULL
      add_filter(expr(!is.na(!!lat_col) & !is.na(!!lon_col)))

      if (!is.null(parsed_filters$location$min_latitude)) {
        add_filter(expr(!!lat_col >= !!parsed_filters$location$min_latitude))
      }
      if (!is.null(parsed_filters$location$max_latitude)) {
        add_filter(expr(!!lat_col <= !!parsed_filters$location$max_latitude))
      }
      if (!is.null(parsed_filters$location$min_longitude)) {
        add_filter(expr(!!lon_col >= !!parsed_filters$location$min_longitude))
      }
      if (!is.null(parsed_filters$location$max_longitude)) {
        add_filter(expr(!!lon_col <= !!parsed_filters$location$max_longitude))
      }
    }

    # APC taxon distribution
    if (parsed_filters$location$location == "apc" && !is.null(parsed_filters$location$apc_taxon_distribution)) {
      pattern <- paste(escape_regex(parsed_filters$location$apc_taxon_distribution), collapse = "|")
      add_filter(expr(stringr::str_detect(taxon_distribution, !!pattern)))
    }
  }

  # 5. CUSTOM FILTERS

  for (custom_filter in parsed_filters$custom) {
    if (is.null(custom_filter)) next
    column <- custom_filter$column
    values <- custom_filter$value

    if (column %in% controlled_vocab_columns) {
      if (column == "dataset_id") {
        # Species averages hold "; "-joined dataset IDs; match whole IDs only
        pattern <- list_element_pattern(values)
        add_filter(expr(stringr::str_detect(!!rlang::sym(column), !!pattern)))
      } else {
        add_filter(expr(!!rlang::sym(column) %in% !!values))
      }
    } else {
      pattern <- free_text_pattern(values)
      if (!is.null(pattern)) {
        add_filter(expr(stringr::str_detect(tolower(!!rlang::sym(column)), !!pattern)))
      }
    }
  }

  # APPLY ALL FILTERS

  if (length(filter_expressions) > 0) {
    cat("[FILTER] Applying combined filter expression\n")

    # Log expressions for debugging
    txt <- purrr::map_chr(filter_expressions, rlang::expr_text) |>
      paste(collapse = " &\n\t ")
    cat("\t", txt, "\n")

    # Apply all filters at once using splice operator
    data <- data |> dplyr::filter(!!!filter_expressions)
  } else {
    cat("[FILTER] No filters to apply\n")
  }

  return(data)
}
