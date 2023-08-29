# Load the testthat package
library(testthat)

# Mock a minimal PipelineResults object
mock_pipelineresults <- readRDS("./fixtures/PipelineResults.rds")

# Test 1: Check if the function returns a list of ggplot objects
test_that("plot_feature_importance returns a list of ggplot objects", {
  plots <- plot_feature_importance(mock_pipelineresults)
  expect_type(plots, "list")
  expect_s3_class(plots[[1]], "gg")
})

# Test 2: Check if the list length matches the number of methods in inbuilt_feature_importance
test_that("List length matches number of methods", {
  plots <- plot_feature_importance(mock_pipelineresults)
  expect_equal(length(plots), length(mock_pipelineresults@inbuilt_feature_importance))
})

# Test 3: Check if it handles the absence of permutation_importance correctly
test_that("Handles absence of permutation_importance correctly", {
  mock_pipelineresults_no_perm <- mock_pipelineresults
  mock_pipelineresults_no_perm@permutation_importance <- list()  # Use an empty list instead of NULL
  plots <- plot_feature_importance(mock_pipelineresults_no_perm)
  expect_type(plots, "list")
})
