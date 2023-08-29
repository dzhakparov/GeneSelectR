# Load the testthat package
library(testthat)

# Load the PipelineResults object from the fixture
pipeline_results_fixture <- readRDS("./fixtures/PipelineResults.rds")

# Create custom lists
custom_lists <- list(
  custom1 = c("feature1", "feature2"),
  custom2 = c("feature3", "feature4")
)

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

# Test 4: Check if it handles custom lists correctly
test_that("Handles custom lists correctly", {
  result_with_custom <- calculate_overlap_coefficients(pipeline_results_fixture, custom_lists)
  expect_type(result_with_custom, "list")
  expect_type(result_with_custom[[1]], "list")
})
