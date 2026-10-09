# Taxon View synonyms from the Australian Plant Census (issue #112)

test_that("#112: synonyms are matched by APC taxon ID, then by name", {
  apc <- data.frame(
    taxon_ID = c("id:A", "id:A2", "id:A3", "id:A4", "id:A5", "id:B", "id:B2", "id:C"),
    accepted_name_usage_ID = c("id:A", "id:A", "id:A", "id:A", "id:A", "id:B", "id:B", "id:C"),
    canonical_name = c("Alpha one", "Alpha uno", "Beta wrong", "Alpha one", "Alpha old", "Gamma two", "Gamma deux", "Delta"),
    scientific_name = c("Alpha one L.", "Alpha uno Sm.", "Beta wrong auct.", "Alpha one L.", "Alpha old F.M.", "Gamma two R.Br.", "Gamma deux DC.", "Delta X."),
    taxonomic_status = c("accepted", "taxonomic synonym", "misapplied", "orthographic variant", "excluded", "accepted", "basionym", "accepted"),
    stringsAsFactors = FALSE
  )
  taxa <- data.frame(
    taxon_name = c("Alpha one", "Gamma two", "Epsilon"),
    # Alpha one matches by ID; Gamma two's ID changed, so it matches by name
    taxon_id = c("id:A", "id:OLD", "id:E"),
    stringsAsFactors = FALSE
  )
  syn <- build_taxon_synonyms(taxa, list(APC = apc), apc_version = "2026-07-30")

  alpha <- syn[syn$taxon_name == "Alpha one", ]
  expect_setequal(alpha$name, c("Alpha uno", "Beta wrong"))   # not itself, not excluded
  expect_equal(alpha$kind[alpha$name == "Beta wrong"], "misapplied")
  expect_equal(syn$name[syn$taxon_name == "Gamma two"], "Gamma deux")
  expect_false("Epsilon" %in% syn$taxon_name)
  expect_equal(attr(syn, "apc_version"), "2026-07-30")

  md <- format_taxon_synonyms("Alpha one", syn)
  expect_match(md, "<strong>Synonyms</strong>: 1 (APC 2026-07-30)", fixed = TRUE)
  expect_match(md, "<strong>Misapplied names</strong>: 1", fixed = TRUE)
  expect_match(md, "- Alpha uno Sm. (taxonomic synonym)", fixed = TRUE)
  expect_match(format_taxon_synonyms("Epsilon", syn), "none listed in the APC 2026-07-30", fixed = TRUE)
  expect_equal(format_taxon_synonyms("Alpha one", NULL), "")
})

test_that("#112: taxon profiles list APC synonyms", {
  skip_if_no_data()
  skip_if(is.null(taxon_synonyms), "No synonyms.rds in this data build")
  taxon <- taxon_synonyms$taxon_name[taxon_synonyms$kind == "synonym"][1]
  html <- paste(generate_taxon_text(taxon), collapse = "\n") |> commonmark::markdown_html()
  expect_match(html, "<strong>Synonyms</strong>", fixed = TRUE)
  expect_match(html, taxon_synonyms$scientific_name[taxon_synonyms$taxon_name == taxon][1], fixed = TRUE)
})
