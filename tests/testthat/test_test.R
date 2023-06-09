library(testthat)
library(GeneSelectR)
# Define a context for your tests
#context("Testing compare_gene_lists")

# # Define a test
test_that("compare_gene_lists returns expected output", {
  GeneSelectR::skip_if_no_modules(names(import_python_packages()))
  work_dir = getwd()
  exp_subset = readRDS(file = file.path(work_dir, "exp_reduced.rds"))
  exp_subset = exp_subset[,900:1001]
  X = exp_subset %>% dplyr::select(-label)
  y = exp_subset[['label']]

  # Call the function
  results <- GeneSelectR(X_train = X, y_train = y, njobs = 1L)

  # Check that the output is a PipelineResults object
  expect_s4_class(results, "PipelineResults")

  # Check that the list has the expected elements
  expect_named(selection_results, c("fitted_pipelines", "cv_results", "mean_feature_importances", 'test_metrics'))
})
