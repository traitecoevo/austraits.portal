# Tests for text generation functions

test_that("generate_taxon_text requires valid taxon", {
  skip_if_no_data()
  skip_if_not(exists("trait_groups"), "Requires loaded trait_groups data")
  
  # Test would require actual data to be loaded
  # This is more of an integration test
})

test_that("generate_taxon_portal_links creates correct links", {
  taxon_info <- list(
    taxon_name = "Eucalyptus globulus",
    family = "Myrtaceae",
    taxon_distribution = "NSW, Vic, Tas",
    taxon_id = "https://id.biodiversity.org.au/node/apni/2899799"
  )
  
  result <- generate_taxon_portal_links(taxon_info)
  
  expect_s3_class(result, "data.frame")
  expect_true("source" %in% names(result))
  expect_true("url" %in% names(result))
  
  # Check that expected portals are included
  expect_true("APC" %in% result$source)
  expect_true("Flora of Australia" %in% result$source)
  expect_true("ALA" %in% result$source)
  
  # Check NSW Flora is included (taxon distributed in NSW)
  expect_true("NSW Flora" %in% result$source)
  
  # Check Vic Flora is included (taxon distributed in Vic)
  expect_true("Vic Flora" %in% result$source)
})

test_that("generate_taxon_portal_links handles different distributions", {
  # Test with WA only
  taxon_info_wa <- list(
    taxon_name = "Banksia prionotes",
    family = "Proteaceae",
    taxon_distribution = "WA",
    taxon_id = "https://id.biodiversity.org.au/node/apni/1234"
  )
  
  result_wa <- generate_taxon_portal_links(taxon_info_wa)
  expect_true("Florabase" %in% result_wa$source)
  expect_false("NSW Flora" %in% result_wa$source)
  
  # Test with NT only
  taxon_info_nt <- list(
    taxon_name = "Corymbia bleeseri",
    family = "Myrtaceae",
    taxon_distribution = "NT",
    taxon_id = "https://id.biodiversity.org.au/node/apni/5678"
  )
  
  result_nt <- generate_taxon_portal_links(taxon_info_nt)
  expect_true("NT eFlora" %in% result_nt$source)
})

test_that("export_bibtex_for_data creates valid bibtex file", {
  skip("Requires actual bibliography data files to be loaded")
  skip_if_not_installed("RefManageR")
  skip_if_not(exists("data_path"), "Requires data_path to be set")
  
  temp_file <- tempfile(fileext = ".bib")
  
  # Mock refs object would be needed
  # For now, test file creation
  expect_error(
    export_bibtex_for_data(c("key1", "key2"), temp_file),
    NA  # Expect no error if data is available
  )
})

test_that("generate_usage_and_citations_text creates valid HTML", {
  skip_if_not(exists("metatdata"), "Requires metadata to be loaded")
  skip_if_not(exists("sources"), "Requires sources to be loaded")
  
  test_data <- data.frame(
    source_primary_key = "test_key_2020",
    source_primary_citation = "Test Author 2020. Test paper. Test Journal."
  )
  
  result <- generate_usage_and_citations_text(test_data)
  
  expect_type(result, "character")
  # Should contain HTML
  expect_true(grepl("<", result))
  # Should mention AusTraits
  expect_true(grepl("AusTraits", result, ignore.case = TRUE))
})

test_that("generate_trait_profile handles valid trait data", {
  skip("Integration test - requires full trait definitions")
  
  # Would need trait_definitions to be loaded
  # and valid data structure
})

test_that("convert_list_to_df1 converts list to data frame", {
  test_list <- list(
    "key1" = "value1",
    "key2" = "value2",
    "key3" = NULL
  )
  
  result <- convert_list_to_df1(test_list)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true("key" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true(is.na(result$value[result$key == "key3"]))
})
