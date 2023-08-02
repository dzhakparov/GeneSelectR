library(testthat)
library(GeneSelectR)
# Define a context for your tests
#context("Testing compare_gene_lists")

# # Define a test
test_that("compare_gene_lists returns expected output", {
  GeneSelectR::skip_if_no_modules(names(import_python_packages()))
  X <- UrbanRandomSubset %>% select(-treatment)
  y <- UrbanRandomSubset %>% select(treatment)

  selection_results <- GeneSelectR(X,
                                   y,
                                   njobs = -1L,
                                   n_splits = 2L,
                                   max_features = 50L,
                                   calculate_permutation_importance = FALSE)

  # Call the function

  # Check that the output is a PipelineResults object
  expect_s4_class(results, "PipelineResults")

  # Check that the list has the expected elements
  expect_named(selection_results, c("fitted_pipelines", "cv_results", "mean_feature_importances", 'test_metrics','cv_mean_score'))
})
