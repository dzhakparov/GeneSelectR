mock_GO_data <- readRDS("./fixtures/EnrichGO_subset.rds")
# Test compute_GO_child_term_metrics function
test_that("compute_GO_child_term_metrics works", {
  # Skip this test if GO.db is not available
  skip_if_not(requireNamespace("GO.db", quietly = TRUE))

  # Run the function with mock data
  result <- compute_GO_child_term_metrics(GO_data = mock_GO_data, GO_terms = c("GO:0001", "GO:0002"))


  # Test 1 Check if the function returns a data.frame
  expect_is(result, "data.frame")

  # Test 2 Check if the data.frame has the expected columns
  expect_setequal(names(result), c("feature_list", "all_terms_number", "offspring_nodes_number", "offspring_terms", "fraction", "GO_term", 'p_value'))

  # Test 3 check if the object has expected number of rows
  expect_equal(nrow(result), 12)

  # Test 4 Check if the function handles invalid ontology
  expect_error(compute_GO_child_term_metrics(GO_data = mock_GO_data, GO_terms = c("GO:0001"), ontology = "INVALID"))
})
