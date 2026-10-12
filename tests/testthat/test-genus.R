# Genus of hybrid formulas starting with a bracket (issue #113)

test_that("#113: a hybrid formula's genus has no leading bracket", {
  skip_if_no_data()
  expect_false(any(grepl("^[(]", dropdowns$all_genus)))
  expect_false(any(duplicated(dropdowns$all_genus)))
  bracketed <- austraits_display_duckdb |>
    dplyr::filter(dplyr::sql("starts_with(genus, '(')")) |>
    dplyr::count() |> dplyr::collect() |> dplyr::pull(n)
  expect_equal(bracketed, 0)
  # The hybrid "(Dockrillia ... x ...) x ..." is found under Dockrillia
  hybrid <- dropdowns$all_taxon_names[grepl("^[(]Dockrillia", dropdowns$all_taxon_names)]
  skip_if(length(hybrid) == 0, "No bracketed Dockrillia hybrid in this data build")
  in_genus <- austraits_display_duckdb |>
    dplyr::filter(genus == "Dockrillia", taxon_name %in% !!hybrid) |>
    dplyr::count() |> dplyr::collect() |> dplyr::pull(n)
  expect_gt(in_genus, 0)
})
