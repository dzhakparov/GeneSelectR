library(testthat)
library(GeneSelectR)
# Define a context for your tests
context("Testing fit_and_evaluate_pipelines")

# # Define a test
test_that("fit_and_evaluate_pipelines returns expected output", {
  skip_if_no_modules(names(import_python_packages()))
  work_dir = getwd()
  exp_subset = readRDS(file = file.path(work_dir, "exp_reduced.rds"))
  exp_subset = exp_subset[,900:1001]
  X = exp_subset %>% dplyr::select(-label)
  y = exp_subset[['label']]

  # Call the function
  results <- fit_and_evaluate_pipelines(X_train = X, y_train = y, njobs = 1L)

  # Check that the output is a PipelineResults object
  expect_s4_class(results, "PipelineResults")

  # Check that the list has the expected elements
  expect_named(results, c("fitted_pipelines", "cv_results", "selected_features", "mean_performance", "mean_feature_importances"))
})

# # Define a test for each feature selection method
# test_that("fit_and_evaluate_pipelines works with Lasso feature selection", {
#   #skip_if_no_modules(names(import_python_packages()))
#   work_dir = getwd()
#   exp_subset = readRDS(file = file.path(work_dir, "exp_reduced.rds"))
#   exp_subset = exp_subset[,900:1001]
#   X = exp_subset %>% dplyr::select(-label)
#   y = exp_subset[['label']]
#   fs_methods <- 'Lasso'
#   results <- fit_and_evaluate_pipelines(X_train = X,
#                                         y_train = y,
#                                         selected_methods = fs_methods,
#                                         njobs = 1L)
#   expect_is(results, "list")
#   expect_named(results, c("fitted_pipelines", "cv_results", "selected_features", "mean_performance", "mean_feature_importances"))
# })
#
# test_that("fit_and_evaluate_pipelines works with Univariate feature selection", {
#   skip_if_no_modules(names(import_python_packages()))
#   fs_methods <- 'Univariate'
#   results <- fit_and_evaluate_pipelines(X_train = X,
#                                         y_train = y,
#                                         selected_methods = fs_methods,
#                                         njobs = 1L)
#   expect_is(results, "list")
#   expect_named(results, c("fitted_pipelines", "cv_results", "selected_features", "mean_performance", "mean_feature_importances"))
# })
#
# test_that("fit_and_evaluate_pipelines works with RFE feature selection", {
#   skip_if_no_modules(names(import_python_packages()))
#   work_dir = getwd()
#   exp_subset = readRDS(file = file.path(work_dir, "exp_reduced.rds"))
#   exp_subset = exp_subset[,900:1001]
#   X = exp_subset %>% dplyr::select(-label)
#   y = exp_subset[['label']]
#   fs_methods <- 'RFE'
#   results <- fit_and_evaluate_pipelines(X_train = X,
#                                         y_train = y,
#                                         selected_methods = fs_methods,
#                                         njobs = 1L)
#   expect_is(results, "list")
#   expect_named(results, c("fitted_pipelines", "cv_results", "selected_features", "mean_performance", "mean_feature_importances"))
# })
#
# test_that("fit_and_evaluate_pipelines works with Boruta feature selection", {
#   skip_if_no_modules(names(import_python_packages()))
#   work_dir = getwd()
#   exp_subset = readRDS(file = file.path(work_dir, "exp_reduced.rds"))
#   exp_subset = exp_subset[,900:1001]
#   X = exp_subset %>% dplyr::select(-label)
#   y = exp_subset[['label']]
#   fs_methods <- 'boruta'
#   results <- fit_and_evaluate_pipelines(X_train = X,
#                                         y_train = y,
#                                         selected_methods = fs_methods,
#                                         njobs = 1L)
#   expect_is(results, "list")
#   expect_named(results, c("fitted_pipelines", "cv_results", "selected_features", "mean_performance", "mean_feature_importances"))
# })
#
# # Define a test for a feature selection method imported from sklearn
# test_that("fit_and_evaluate_pipelines works with sklearn's SelectKBest (not included in default set) feature selection", {
#   skip_if_no_modules(names(import_python_packages()))
#   work_dir = getwd()
#   exp_subset = readRDS(file = file.path(work_dir, "exp_reduced.rds"))
#   exp_subset = exp_subset[,900:1001]
#   X = exp_subset %>% dplyr::select(-label)
#   y = exp_subset[['label']]
#   # Import the SelectKBest feature selection method from sklearn
#   SelectKBest <- sklearn$feature_selection$SelectKBest
#
#   # Create a list with the SelectKBest feature selection method
#   fs_methods <- list("SelectKBest" = SelectKBest(sklearn$feature_selection$f_classif, k=10))
#
#   # Call the function with the SelectKBest feature selection method
#   results <- fit_and_evaluate_pipelines(X_train = X,
#                                         y_train = y,
#                                         feature_selection_methods = fs_methods,
#                                         njobs = 1L)
#
#   # Check the output
#   expect_is(results, "list")
#   expect_named(results, c("fitted_pipelines", "cv_results", "selected_features", "mean_performance", "mean_feature_importances"))
# })
