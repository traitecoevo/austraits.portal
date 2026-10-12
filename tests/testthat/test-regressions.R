# Regression tests for the bugs in issue #130 (October 2026 review), one or more per
# bug. The B1 and B2 data tests were confirmed to fail on the reviewed code;
# the others exercise the helpers that replaced it. They run against whichever
# data build is loaded (the committed lite build on CI).

empty_filters <- function(...) {
  # Not modifyList(): it drops the unnamed elements of `custom`
  filters <- list(taxon = list(), trait = list(), location = list(), other = list(), custom = list())
  args <- list(...)
  filters[names(args)] <- args
  filters
}

count_rows <- function(query) {
  query |> dplyr::count() |> dplyr::collect() |> dplyr::pull(n)
}

quietly <- function(expr) {
  utils::capture.output(result <- expr)
  result
}

test_that("B1: taxon names containing brackets are matched exactly", {
  skip_if_no_data()
  bracketed <- dropdowns$all_taxon_names[grepl("[][()]", dropdowns$all_taxon_names)]
  skip_if(length(bracketed) == 0, "No bracketed taxon names in this data build")

  for (x in utils::head(bracketed, 10)) {
    filtered <- quietly(apply_filters(austraits_display_duckdb,
      empty_filters(taxon = list(taxon_type = "taxon_name", taxon_name = x))))
    truth <- austraits_display_duckdb |> dplyr::filter(taxon_name == !!x)
    expect_equal(count_rows(filtered), count_rows(truth), label = x)
  }
})

test_that("B2: multi-value custom filters work and dataset_id matches whole IDs", {
  skip_if_no_data()
  stages <- austraits_display_duckdb |> dplyr::distinct(life_stage) |> dplyr::collect() |>
    dplyr::pull() |> stats::na.omit() |> utils::head(2)
  skip_if(length(stages) < 2, "Fewer than two life stages in this data build")

  filtered <- quietly(apply_filters(austraits_display_duckdb, empty_filters(
    custom = list(list(column = "life_stage", value = stages)))))
  expect_gt(count_rows(filtered), 0)

  # IDs that are prefixes of other IDs must not pull those in
  ids <- austraits_display_duckdb |> dplyr::distinct(dataset_id) |> dplyr::collect() |> dplyr::pull()
  prefixes <- ids[vapply(ids, function(i) sum(startsWith(ids, i)) > 1, logical(1))]
  skip_if(length(prefixes) == 0, "No prefix dataset IDs in this data build")
  filtered <- quietly(apply_filters(austraits_display_duckdb, empty_filters(
    custom = list(list(column = "dataset_id", value = prefixes[1])))))
  got <- filtered |> dplyr::distinct(dataset_id) |> dplyr::collect() |> dplyr::pull()
  expect_equal(got, prefixes[1])
})

test_that("B2: dataset_id matches whole elements of species-average ID lists", {
  pattern <- list_element_pattern("Bryant_2021")
  expect_true(grepl(pattern, "Bryant_2021"))
  expect_true(grepl(pattern, "Abc_2001; Bryant_2021"))
  expect_true(grepl(pattern, "Bryant_2021; Abc_2001"))
  expect_false(grepl(pattern, "Bryant_2021_2"))
  expect_false(grepl(pattern, "Abc_2001; Bryant_2021_2"))
})

test_that("B2: free-text filters treat input literally, with | as 'or'", {
  expect_null(free_text_pattern(" | "))
  pattern <- free_text_pattern("Genus-Level|(bogus")
  expect_true(grepl(pattern, "inferred from genus-level description"))
  expect_true(grepl(pattern, "see (bogus) note"))
  expect_false(grepl(pattern, "bogus"))
  skip_if_no_data()
  expect_no_error(quietly(apply_filters(austraits_display_duckdb, empty_filters(
    custom = list(list(column = "location_name", value = "(")))) |> count_rows()))
})

test_that("B2: trait features with no matching trait return no rows", {
  skip_if_no_data()
  filtered <- quietly(apply_filters(austraits_display_duckdb, empty_filters(
    trait = list(trait_filter_type = "features", structure_measured = "no-such-structure"))))
  expect_equal(count_rows(filtered), 0)
})

test_that("B3: the two Apply buttons have distinct ids", {
  html <- as.character(bslib::page_sidebar(sidebar = mod_filters_ui("filters")))
  expect_equal(lengths(regmatches(html, gregexpr('id="filters-apply_filters_btn"', html))), 1)
  expect_equal(lengths(regmatches(html, gregexpr('id="filters-apply_filters_btn_bottom"', html))), 1)
})

test_that("B4: Taxon View survives an empty (cleared) result", {
  skip_if_no_data()
  taxon <- dropdowns$all_taxon_names[1]
  local_mocked_bindings(generate_taxon_text_cached = function(x) generate_taxon_text(x))
  testServer(mod_taxon_view_server, args = list(
    filters = reactive(list(taxon_type = "taxon_name", taxon_name = taxon)),
    current_tab = reactive("Taxon View"),
    apply_filters_trigger = reactiveVal(1)
  ), {
    session$flushReact()
    expect_no_error(output$taxon_text)
  })
})

test_that("B5: a taxon profile renders for a real taxon", {
  skip_if_no_data()
  # A taxon with categorical trait data exercises the value_range column
  taxon <- austraits_species_duckdb |>
    dplyr::filter(type == "categorical") |>
    dplyr::distinct(taxon_name) |>
    utils::head(1) |>
    dplyr::collect() |>
    dplyr::pull()
  skip_if(length(taxon) == 0, "No categorical species averages in this data build")
  txt <- generate_taxon_text(taxon)
  expect_type(txt, "character")
  expect_true(any(grepl(taxon, txt, fixed = TRUE)))
})

test_that("B5: loaded data has every required column", {
  skip_if_no_data()
  expect_no_error(check_data_schema(colnames(austraits_display_duckdb), data_schema$display, "display"))
  expect_no_error(check_data_schema(colnames(austraits_species_duckdb), data_schema$species, "species"))
  expect_no_error(check_data_schema(colnames(austraits_duckdb), data_schema$data, "data"))
  expect_error(check_data_schema(c("taxon_name"), data_schema$display, "display"), "missing required columns")
})

test_that("B6: the data table escapes HTML in cell text", {
  # Context descriptions are written "<<...>>"; unescaped, a browser drops
  # them as unknown tags
  data <- data.frame(taxon_name = "A", trait_name = "t",
                     method_context_properties = "x <<Trait value inferred from genus.>>")
  attr(data, "total_rows") <- 1
  escape <- NULL
  real_datatable <- DT::datatable
  local_mocked_bindings(
    datatable = function(data, ..., escape = TRUE) {
      escape <<- escape
      real_datatable(data, ..., escape = escape)
    },
    .package = "DT"
  )
  testServer(mod_data_table_server, args = list(
    filtered_database = reactiveVal(data),
    filtered_query_cache = reactiveVal(NULL),
    columns_display_reactive = reactive(names(data))
  ), {
    output$data_table
    expect_true(isTRUE(escape))
  })
})

test_that("B7: only user searches are logged, not dataset switches or paging", {
  skip_if_no_data()
  logged <- character()
  local_mocked_bindings(
    log_telemetry_event = function(event_type, details = list()) logged <<- c(logged, event_type),
    start_telemetry_session = function() invisible(NULL)
  )
  testServer(app_server, {
    session$setInputs(
      `filters-dataset_type` = "species", `filters-taxon_type` = "all",
      `filters-trait_filter_type` = "name", `filters-location` = ""
    )
    session$flushReact()
    # The mock session carries URL parameters, so startup takes the link path,
    # which counts as one search; count from there
    baseline <- sum(logged == "search")
    session$setInputs(`filters-apply_filters_btn` = 1)
    expect_equal(sum(logged == "search"), baseline + 1)
    session$setInputs(`filters-apply_filters_btn_bottom` = 1)
    expect_equal(sum(logged == "search"), baseline + 2)
    session$setInputs(`filters-dataset_type` = "raw")
    expect_equal(sum(logged == "search"), baseline + 2)   # dataset switch is not a search
  })
})

test_that("Telemetry is off under testthat", {
  expect_true(telemetry_disabled())
})

test_that("Shiny does not auto-source R/ when serving app.R", {
  # Shiny Server runs app.R after sourcing R/ into the app's environment,
  # unless R/_disable_autoload.R exists. Those sourced copies would see only
  # the empty data placeholders, and every filter would fail.
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "Not running from a source checkout")
  expect_true(file.exists(file.path(r_dir, "_disable_autoload.R")))
})
