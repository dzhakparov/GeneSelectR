#' @title Fit and Evaluate Pipelines with Hyperparameter Tuning
#' @description This function fits a set of pipelines with hyperparameter tuning on a given training set and evaluates their performance using cross-validation.
#' @importFrom reticulate import r_to_py
#' @importFrom glue glue
#' @importFrom dplyr bind_rows
#' @param X_train A matrix or data frame of training data with features as columns and observations as rows.
#' @param y_train A vector of training labels corresponding to the rows of X_train.
#' @param pipelines An optional list of pre-defined pipelines to use for fitting and evaluation. If this argument is provided, the feature selection methods and preprocessing steps will be ignored.
#' @param feature_selection_methods An optional list of feature selection methods to use for fitting and evaluation. If this argument is not provided, a default set of feature selection methods will be used.
#' @param selected_methods An optional vector of names of feature selection methods to use from the default set. If this argument is provided, only the specified methods will be used.
#' @param testsize test set size
# @param condaenv a conda environment for the feature selection procedure.
#' @return A list with the following elements:
#' \item{fitted_pipelines}{A list of the fitted pipelines.}
#' \item{cv_results}{A list of the cross-validation results for each pipeline.}
#' @examples
#' \dontrun{
#' # Fit and evaluate pipelines using the default feature selection methods
#' data(iris)
#' X <- iris[,1:4]
#' y <- iris[,5]
#' results <- fit_and_evaluate_pipelines(X_train = X, y_train = y)
#'
#' # Fit and evaluate pipelines using a subset of the default feature selection methods
#' results <- fit_and_evaluate_pipelines(X_train = X, y_train = y, selected_methods = c("Univariate", "RFE"))
#'
#' # Fit and evaluate pipelines using user-defined feature selection methods
#' fs_methods <- list("Lasso" = select_model(lasso(penalty = 'l1', C = 0.1, solver = 'saga'), threshold = 'median'))
#' results <- fit_and_evaluate_pipelines(X_train = X, y_train = y, feature_selection_methods = fs_methods)
#'}
#' @export
fit_and_evaluate_pipelines <- function(X_train,
                                       y_train,
                                       pipelines = NULL,
                                       feature_selection_methods = NULL,
                                       selected_methods = NULL,
                                       testsize = 0.2,
                                       #condaenv,
                                       njobs = -1L,
                                       n_splits = 2L) {

  # enable multiprocess on Windows machines
  if (Sys.info()["sysname"] == "Windows") {
    exe <- file.path(sys$exec_prefix, "pythonw.exe")
    sys$executable <- exe
    sys$`_base_executable` <- exe
    multiprocessing$set_executable(exe)
  }


  # define Python modules and sklearn submodules
  preprocessing <- sklearn$preprocessing
  model_selection <- sklearn$model_selection
  feature_selection <- sklearn$feature_selection
  ensemble <- sklearn$ensemble
  pipeline <- sklearn$pipeline$Pipeline

  # define default models for the feature selection process
  forest <- sklearn$ensemble$RandomForestClassifier
  grid <- sklearn$model_selection$RandomizedSearchCV
  lasso <- sklearn$linear_model$LogisticRegression
  univariate <- sklearn$feature_selection$GenericUnivariateSelect
  select_model <- sklearn$feature_selection$SelectFromModel

  # define a classifier for the accuracy estimation
  GradBoost <- ensemble$GradientBoostingClassifier

  # Define preprocessing steps as a list
  preprocessing_steps <- list(
    "VarianceThreshold" = sklearn$feature_selection$VarianceThreshold(),
    "MinMaxScaler" = sklearn$preprocessing$MinMaxScaler()
  )

  # Create a list of default feature selection methods
  default_feature_selection_methods <- list(
    "Lasso" = select_model(lasso(penalty = 'l1', C = 0.1, solver = 'saga'), threshold = 'median'),
    'Univariate' = univariate(mode = 'percentile',param = 80L),
    'boruta'= boruta$BorutaPy(forest(), n_estimators = 'auto', verbose =0, random_state = 999L,perc = 90L)
  )

  # convert R objects to Py
  X_train <- reticulate::r_to_py(X_train)
  y_train <- reticulate::r_to_py(y_train)
  y_train <- y_train$values$ravel()

  # Use the default feature selection methods if none are provided
  if (is.null(feature_selection_methods)) {
    feature_selection_methods <- default_feature_selection_methods
  }

  # Select the specified feature selection methods if they are provided
  if (!is.null(selected_methods)) {
    feature_selection_methods <- feature_selection_methods[selected_methods]
  }

  # Select the specified pipelines if they are provided
  if (!is.null(pipelines)) {
    selected_pipelines <- pipelines
  } else {
    # Define the default pipelines using the selected feature selection methods
    selected_pipelines <- create_pipelines(feature_selection_methods,
                                           preprocessing_steps,
                                           classifier = GradBoost())
  }

  fitted_pipelines <- list()
  cv_results <- list()
  selected_features <- list()
  split_results <- list()
  mean_performances <- list()
  test_metrics <- list()

  # Repeated train-test splitting
  for (split_idx in 1:n_splits) {
    message(glue::glue("Split {split_idx} \n"))
    split_data <- model_selection$train_test_split(X_train, y_train, test_size = testsize)
    X_train_split <- reticulate::r_to_py(split_data[[1]])
    X_test_split <- reticulate::r_to_py(split_data[[2]])
    y_train_split <- reticulate::r_to_py(split_data[[3]])
    y_test_split <- reticulate::r_to_py(split_data[[4]])

    split_fitted_pipelines <- list()
    split_cv_results <- list()
    split_selected_features <- list()
    split_mean_performances <- list()
    split_test_metrics <- list()

    for (i in seq_along(names(selected_pipelines))) {
      message(glue("Fitting {names(selected_pipelines)[[i]]} \n"))

      # Create the parameter grid with the 'classifier' prefix
      params <- setNames(
        list(seq(50L, 200L, 50L), seq(3L, 7L, 2L)),
        c("classifier__n_estimators", "classifier__max_depth")
      )

      # Hyperparameter tuning using GridSearchCV
      grid_search_cv <- grid(
        estimator = selected_pipelines[[i]],
        param_distributions = params,
        cv=5L,
        n_jobs = njobs,
        verbose = 2L)
      grid_search_cv$fit(X_train_split, y_train_split)


      # Evaluate the best model on the test set
      best_model <- grid_search_cv$best_estimator_
      y_pred <- best_model$predict(X_test_split)
      precision <- sklearn$metrics$precision_score(y_test_split, y_pred, average = "weighted")
      recall <- sklearn$metrics$recall_score(y_test_split, y_pred, average = "weighted")
      f1 <- sklearn$metrics$f1_score(y_test_split, y_pred, average = "weighted")
      accuracy <- sklearn$metrics$accuracy_score(y_test_split, y_pred)

      # Save the mean test score for the current split
      # This mean test score refers to the mean test score during cross validation
      mean_test_score <- mean(grid_search_cv$cv_results_$mean_test_score, na.rm = TRUE)

      # Save the fitted pipeline and results for the current split
      split_fitted_pipelines[[names(selected_pipelines)[[i]]]] <- grid_search_cv$best_estimator_
      split_cv_results[[names(selected_pipelines)[[i]]]] <- grid_search_cv$cv_results_
      split_selected_features[[names(selected_pipelines)[[i]]]] <- get_feature_importances(split_fitted_pipelines[[i]], X_train_split)
      split_mean_performances[[names(selected_pipelines)[[i]]]] <- mean_test_score
      split_test_metrics[[names(selected_pipelines)[[i]]]] <- list(precision = precision, recall = recall, f1 = f1, accuracy = accuracy) # save the other metrics for the current split
    }


    # Append the results of the current split to the main results list
    split_results[[glue::glue('split_{split_idx}')]] <- split_fitted_pipelines
    selected_features[[glue::glue('split_{split_idx}')]] <- split_selected_features
    cv_results[[glue::glue('split_{split_idx}')]] <- split_cv_results
    mean_performances[[glue::glue('split_{split_idx}')]] <- split_mean_performances
    test_metrics[[glue::glue('split_{split_idx}')]] <- split_test_metrics
  }

  # Convert the nested list into a data frame
  test_metrics_df <- test_metrics %>%
    tibble::enframe(name = "split", value = "methods") %>%
    tidyr::unnest_longer(methods, indices_to = "method") %>%
    tidyr::unnest_wider(methods)

  # Compute means and standard deviations
  test_metrics_df <- test_metrics_df %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(across(c(dplyr::starts_with("f1"), dplyr::starts_with("recall"),
                              dplyr::starts_with("precision"), dplyr::starts_with("accuracy")),
                            list(mean = mean, sd = sd), .names = "{.col}_{.fn}"))


  # Print the summary data frame
  print(test_metrics_df)
  # Bind the data frames together
  # test_metrics_df

  # Calculate the mean performance for each feature selection method across all splits
  mean_performance_df <- do.call(rbind, lapply(mean_performances, as.data.frame))

  # Calculate the mean feature importance for each method across all splits
  mean_feature_importances <- aggregate_feature_importances(selected_features)


  return(new("PipelineResults",
             fitted_pipelines = split_results,
             cv_results = cv_results,
             mean_feature_importances = mean_feature_importances,
             test_metrics = test_metrics_df))


}

