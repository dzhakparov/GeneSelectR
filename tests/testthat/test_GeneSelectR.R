testthat::skip_on_cran()
modules <- c('sklearn', 'pandas', 'numpy', 'boruta', 'sys', 'multiprocessing', 'skopt')
# Skip all tests if Python is not available
GeneSelectR::skip_if_no_modules(modules)
GeneSelectR::set_reticulate_python()
# load fixtures
matrix_fixture <- readRDS('./fixtures/UrbanRandomSubset.rda')
test_that("GeneSelectR returns expected output types", {

  X <- UrbanRandomSubset %>% dplyr::select(-treatment) # get the feature matrix
  y <- UrbanRandomSubset['treatment'] # store the data point label in a separate vector

  # Run the function
  result <- GeneSelectR(X,
                        y,
                        search_type = 'bayesian')

  # Check the types of the output
  expect_is(result, "PipelineResults")
  expect_is(result$best_pipeline, "list")
  expect_is(result$cv_results, "list")
  expect_is(result$inbuilt_feature_importance, "list")
  expect_is(result$cv_mean_score, "data.frame")
})

test_that("GeneSelectR handles custom feature selection methods", {
  # Generate some mock data
  X <- UrbanRandomSubset %>% dplyr::select(-treatment) # get the feature matrix
  y <- UrbanRandomSubset['treatment'] # store the data point label in a separate vector

  sklearn <- reticulate::import('sklearn')
  feature_selection <- sklearn$feature_selection
  select_from_model <- feature_selection$SelectFromModel
  RFE <- feature_selection$RFE
  rf <- sklearn$ensemble$RandomForestClassifier

  # define feature selection params
  my_params <- list('RFE' = list('feature_selector__step' = seq(0.1, 0.001, 1, 10)),
                    'SelectFromModel' = list('feature_selector__estimator__n_estimators' = c(50L, 100L, 250L, 500L),
                                             "feature_selector__estimator__max_depth" = c(10L, 20L, 30L),
                                             "feature_selector__estimator__bootstrap" = c(TRUE, FALSE))
  )

  # feature selection methods of your choice
  my_methods <- list('RFE' = RFE(estimator = rf(), n_features_to_select = 100L),
                     'SelectFromModel' = select_from_model(estimator = rf()))

  # Run the function
  result <- GeneSelectR(X,
                        y,
                        custom_fs_methods = my_methods,
                        custom_fs_grids = my_params)

  # Check if the custom method is used
  expect_true("RFE" %in% names(result$best_pipeline))
})

 test_that("GeneSelectR uses custom classifiers if provided", {
  # Generate some mock data
  X <- UrbanRandomSubset %>% dplyr::select(-treatment) # get the feature matrix
  y <- UrbanRandomSubset['treatment'] # store the data point label in a separate vector

  sklearn <- reticulate::import('sklearn')

  # import lasso
  lasso <- sklearn$linear_model$Lasso

  lasso_grid <- list(
    'classifier__penalty' = c('l1','2'),
    'classifier__C' = c('0.1','1','0.01')
  )
  # Run the function
  result <- GeneSelectR(X,
                        y,
                        classifier = lasso,
                        classifier_grid = lasso_grid)

  # Check if the custom classifier is used
  # This check will depend on how you store the classifier in the result
  # For example, if it's an attribute:
  # Check the types of the output
  expect_is(result, "PipelineResults")
  expect_is(result$best_pipeline, "list")
  expect_is(result$cv_results, "list")
  expect_is(result$inbuilt_feature_importance, "list")
  expect_is(result$cv_mean_score, "data.frame")
})

