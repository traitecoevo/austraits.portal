test_that("app ui works", {
  ui <- app_ui()
  expect_type(ui, "list")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("app server works", {
  server <- app_server
  expect_type(server, "closure")
  # Check that formals have correct parameters
  fmls <- formals(app_server)
  expect_true("input" %in% names(fmls))
  expect_true("output" %in% names(fmls))
  expect_true("session" %in% names(fmls))
})

test_that("app launches", {
  # Simple test that app doesn't crash on startup
  expect_error(app_ui(), NA)
})

test_that("app has required dependencies", {
  # Check that required packages are available
  expect_true(requireNamespace("shiny", quietly = TRUE))
  expect_true(requireNamespace("bslib", quietly = TRUE))
  expect_true(requireNamespace("dplyr", quietly = TRUE))
  expect_true(requireNamespace("arrow", quietly = TRUE))
  expect_true(requireNamespace("DT", quietly = TRUE))
})

test_that("all modules have matching ui and server functions", {
  modules <- c("filters", "data_table", "taxon_view", "trait_view", "citations", "app_info")
  
  for (mod in modules) {
    ui_func <- paste0("mod_", mod, "_ui")
    server_func <- paste0("mod_", mod, "_server")
    
    expect_true(exists(ui_func), info = paste("Missing", ui_func))
    expect_true(exists(server_func), info = paste("Missing", server_func))
  }
})

test_that("app has proper structure", {
  # Basic structure checks
  expect_true(exists("app_ui"))
  expect_true(exists("app_server"))
  expect_type(app_ui, "closure")
  expect_type(app_server, "closure")
})

test_that("data loading variables are properly set", {
  skip_if_not(exists("columns_display"), "Data not loaded")
  
  expect_type(columns_display, "character")
  expect_true(length(columns_display) > 0)
})

test_that("app handles reactive values correctly", {
  testServer(app_server, {
    # Test that reactive values are initialized
    expect_true(exists("filtered_total"))
    expect_true(exists("filtered_query_cache"))
  })
})
