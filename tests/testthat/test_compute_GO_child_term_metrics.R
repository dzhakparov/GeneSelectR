# Create a mock GO_data list
mock_GO_data <- readRDS('./fixtures/EnrichGO.rds')


# Test compute_GO_child_term_metrics function
test_that("compute_GO_child_term_metrics works", {

  # Skip this test if GO.db is not available
  skip_if_not(requireNamespace("GO.db", quietly = TRUE))

  # Run the function
  result <- compute_GO_child_term_metrics(GO_data = mock_GO_data, GO_terms = c("GO:0001", "GO:0002"))
  print(str(result))

  # Check if the function returns a data.frame
  expect_is(result, "data.frame")

  # Check if the data.frame has the expected columns
  expect_setequal(names(result), c("feature_list", "all_terms_number", "offspring_nodes_number", "offspring_terms", "fraction", "GO_term"))

  # Check if the data.frame has the expected number of rows
  expect_equal(nrow(result), 12)  # 2 feature lists * 2 GO terms

  # Check if the function handles invalid ontology
  expect_error(compute_GO_child_term_metrics(GO_data = mock_GO_data, GO_terms = c("GO:0001"), ontology = "INVALID"))
})
