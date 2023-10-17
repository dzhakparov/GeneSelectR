
# Load the PipelineResults object from the fixture
pipeline_results_fixture <- readRDS("./fixtures/PipelineResults.rds")

# Test 1: Check if the function returns a list of lists of matrices
test_that("calculate_overlap_coefficients returns a list of lists of matrices", {
  result <- calculate_overlap_coefficients(pipeline_results_fixture)
  expect_type(result, "list")
  expect_type(result[[1]], "list")
  expect_type(result[[1]][["overlap"]], "double")
})

# Test 2: Check if it handles the absence of permutation_importance correctly
test_that("Handles absence of permutation_importance correctly", {
  pipeline_results_no_perm <- pipeline_results_fixture
  pipeline_results_no_perm@permutation_importance <- list()
  result <- calculate_overlap_coefficients(pipeline_results_no_perm)
  expect_type(result, "list")
  expect_type(result[[1]], "list")
  expect_type(result[[1]][["overlap"]], "double")
})

# Test 3: Check if it throws an error for incorrect input types
test_that("Throws an error for incorrect input types", {
  expect_error(calculate_overlap_coefficients(data.frame()), "The input object does not belong to the PipelineResults class.")
})
