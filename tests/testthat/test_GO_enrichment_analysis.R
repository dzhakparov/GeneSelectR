# Load the fixture
annotated_gene_lists_fixture <- readRDS('./fixtures/AnnotatedGeneLists.rds')
# background_fixture
background_fixture <- annotated_gene_lists_fixture@inbuilt[["background"]]@ENTREZID

test_that("GO_enrichment_analysis returns expected results", {
  skip_if_not_installed("clusterProfiler")
  skip_if_offline()

  # Run the function
  result <- GO_enrichment_analysis(
    annotated_gene_lists = annotated_gene_lists_fixture,
    background = background_fixture,
    organism = "org.Hs.eg.db",
    keyType = "ENTREZID",
    minGSSize = 10
  )

  print(result)

  # Check if the result is a list
  expect_is(result, "list")

  # Check if the list is not empty
  expect_true(length(result) > 0)

  expected_names <- c('')
  expect_equal(names(result), expected_names)

  # Add more specific checks based on your expected results
})

test_that("GO_enrichment_analysis throws an error for invalid list_type", {

  skip_if_not_installed("clusterProfiler")
  skip_if_offline()

  expect_error(
    GO_enrichment_analysis(
      annotated_gene_lists = annotated_gene_lists_fixture,
      list_type = "invalid_type",
      background = background_fixture,
      organism = "org.Hs.eg.db",
      keyType = "ENTREZID",
      minGSSize = 1
    ),
    "list_type should be 'inbuilt' or 'permutation'"
  )
})
