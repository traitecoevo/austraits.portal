# Trait View plots with traits.use::plot_trait_distribution(). The plot
# functions are tested in traits.use; these tests check the way the portal
# calls them: a collected table for one trait, with `value` set from
# `value_mean` for species averages, then converted with plotly::ggplotly().

portal_plot <- function(data) {
  traits.use::plot_trait_distribution(data, data$trait_name[1])
}

numeric_rows <- tibble::tibble(
  dataset_id = rep(c("Smith_2020", "Jones_2021"), each = 3),
  taxon_name = c("Acacia a", "Acacia b", "Eucalyptus c", "Acacia a", "Banksia d", "Banksia e"),
  genus = c("Acacia", "Acacia", "Eucalyptus", "Acacia", "Banksia", "Banksia"),
  family = c("Fabaceae", "Fabaceae", "Myrtaceae", "Fabaceae", "Proteaceae", "Proteaceae"),
  trait_name = "leaf_mass_per_area",
  value = c("80", "95", "120", "101", "150", "210"),
  unit = "g/m2",
  value_type = c("mean", "mean", "raw", "minimum", "maximum", "mean")
)

test_that("numeric traits plot by family, with tooltips that survive ggplotly", {
  skip_if_not_installed("plotly")
  p <- portal_plot(numeric_rows)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 6)
  expect_setequal(levels(p$data$Group), c("Fabaceae", "Myrtaceae", "Proteaceae"))
  expect_true("text" %in% names(p$mapping))

  pl <- plotly::plotly_build(plotly::ggplotly(p, tooltip = c("x", "y", "text")))
  tooltips <- unlist(lapply(pl$x$data, `[[`, "text"))
  expect_true(any(grepl("Smith_2020 | Acacia a", tooltips, fixed = TRUE)))
})

test_that("bin and range values are left out of numeric plots", {
  data <- numeric_rows
  data$value[1] <- "50--90"
  data$value_type[1] <- "range"
  expect_equal(nrow(portal_plot(data)$data), 5)
})

test_that("negative numeric traits keep every value inside the axis", {
  # The portal's own copy shrank the limits inwards for negative values,
  # hiding 20% of leaf_delta13C values
  data <- numeric_rows
  data$trait_name <- "leaf_delta13C"
  data$value <- c("-38.8", "-30", "-28", "-25", "-12", "-9.9")
  p <- portal_plot(data)
  x_range <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
  expect_lte(x_range[1], -38.8)
  expect_gte(x_range[2], -9.9)
})

test_that("species averages plot from value_mean", {
  avg <- numeric_rows |>
    dplyr::select(-value) |>
    dplyr::mutate(value_mean = c(80, 95, 120, 101, 150, 210), value_type = "mean")
  data <- avg |> dplyr::mutate(value = value_mean)
  p <- portal_plot(data)
  expect_s3_class(p, "ggplot")
  expect_equal(sort(p$data$value), c(80, 95, 101, 120, 150, 210))
})

test_that("a single value or identical values still plot", {
  expect_s3_class(portal_plot(numeric_rows[1, ]), "ggplot")
  same <- numeric_rows |> dplyr::mutate(value = "10")
  expect_no_error(ggplot2::ggplot_build(portal_plot(same)))
})

test_that("categorical traits (no unit) plot proportions by family", {
  skip_if_not_installed("plotly")
  data <- tibble::tibble(
    dataset_id = rep(c("Smith_2020", "Jones_2021"), each = 3),
    taxon_name = c("Acacia a", "Acacia b", "Eucalyptus c", "Acacia a", "Banksia d", "Banksia e"),
    family = c("Fabaceae", "Fabaceae", "Myrtaceae", "Fabaceae", "Proteaceae", "Proteaceae"),
    trait_name = "leaf_compoundness",
    value = c("simple", "compound", "simple", "simple compound", "simple", "simple"),
    unit = NA_character_,
    value_type = "mode"
  )
  p <- portal_plot(data)
  expect_s3_class(p, "ggplot")
  expect_setequal(levels(p$data$value), c("simple", "compound"))
  expect_no_error(plotly::ggplotly(p, tooltip = c("x", "y", "text")))
})
