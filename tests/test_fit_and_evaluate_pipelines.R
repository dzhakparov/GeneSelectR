rm(list = ls())
gc()
library(testthat)
library(FitSelect)
test_check("FitSelect")

# Define a context for your tests
context("Testing fit_and_evaluate_pipelines")


# Define a test
test_that("fit_and_evaluate_pipelines returns expected output", {
  work_dir = getwd()
  exp_subset = readRDS(file = file.path(work_dir, "exp_reduced.rds"))
  exp_subset = exp_subset[,900:1001]

  # Load your data here
  X = exp_subset %>% dplyr::select(-label)
  y = exp_subset[['label']]

  # Call the function
  results <- fit_and_evaluate_pipelines(X_train = X, y_train = y, njobs = 1L)

  # Check that the output is a list
  expect_is(results, "list")

  # Check that the list has the expected elements
  expect_named(results, c("fitted_pipelines", "cv_results", "selected_features", "mean_performance", "mean_feature_importances"))

  # Add more expectations as needed...
})
