#' Define Python modules and scikit-learn submodules
#'
#' @return A list containing the definitions for the Python modules and submodules.
#' @export
define_sklearn_modules <- function() {
  # define Python modules and sklearn submodules
  preprocessing <- sklearn$preprocessing
  model_selection <- sklearn$model_selection
  feature_selection <- sklearn$feature_selection
  ensemble <- sklearn$ensemble
  pipeline <- sklearn$pipeline$Pipeline

  # define default models for the feature selection process
  forest <- sklearn$ensemble$RandomForestClassifier
  randomized_grid <- sklearn$model_selection$RandomizedSearchCV
  grid <- sklearn$model_selection$GridSearchCV
  bayesianCV <- skopt$BayesSearchCV
  lasso <- sklearn$linear_model$LogisticRegression
  univariate <- sklearn$feature_selection$GenericUnivariateSelect
  select_model <- sklearn$feature_selection$SelectFromModel

  # define a classifier for the accuracy estimation
  GradBoost <- ensemble$GradientBoostingClassifier

  return(list(preprocessing = preprocessing,
              model_selection = model_selection,
              feature_selection = feature_selection,
              ensemble = ensemble,
              pipeline = pipeline,
              forest = forest,
              randomized_grid = randomized_grid,
              grid = grid,
              bayesianCV = bayesianCV,
              lasso = lasso,
              univariate = univariate,
              select_model = select_model,
              GradBoost = GradBoost))

}

#' Enable Multiprocessing in Python Environment
#'
#' This function sets up the necessary executable for Python's multiprocessing functionality.
#' Only used on Windows
#'
enable_multiprocess <- function() {
  exe <- file.path(sys$exec_prefix, "pythonw.exe")
  sys$executable <- exe
  sys$`_base_executable` <- exe
  multiprocessing$set_executable(exe)
}

#' Set Default Feature Selection Methods
#'
#' @param modules A list containing the definitions for the Python modules and submodules.
#' @param max_features The maximum number of features to consider.
#' @param random_state An integer value setting the random seed for feature selection algorithms and cross validation procedure. By default set to NULL to use different random seed every time an algorithm is used. For reproducibility could be fixed, otherwise for an unbiased estimation should be left as NULL.
#' @return A list containing preprocessing steps and default feature selection methods.
#'
set_default_fs_methods <- function(modules, max_features, random_state) {
  VarianceThreshold <- modules$feature_selection$VarianceThreshold(0.85)
  StandardScaler <- modules$preprocessing$StandardScaler()
  Lasso <- modules$select_model(modules$lasso(penalty = 'l1', C = 0.01, solver = 'saga', max_iter = 1000L, random_state = random_state), threshold = 'median', max_features = max_features)
  Univariate <- modules$univariate(mode = 'k_best',param = max_features)
  RandomForest <- modules$select_model(modules$forest(n_estimators=100L, random_state=random_state), threshold = 'median', max_features = max_features)
  boruta <- boruta$BorutaPy(modules$forest(random_state = random_state), n_estimators = 'auto', verbose = 0,perc = 100L)

  preprocessing_steps <- list(
    "VarianceThreshold" = VarianceThreshold,
    "StandardScaler" = StandardScaler
  )

  default_feature_selection_methods <- list(
    "Lasso" = Lasso,
    'Univariate' = Univariate,
    'RandomForest' = RandomForest,
    'boruta'= boruta
  )

  return(list(preprocessing_steps = preprocessing_steps,
              default_feature_selection_methods = default_feature_selection_methods))

}

#' Set Default Parameter Grids for Feature Selection
#' @param max_features An integer indicating max_features to select in Univariate select
#' @return A list containing the default parameter grids for feature selection methods.
#'
set_default_param_grids <- function(max_features) {
  # Calculate step size dynamically
  step_size <- ifelse(max_features <= 100L, 10L,
                      ifelse(max_features <= 1000L, 100L,
                             ifelse(max_features <= 10000L, 50L, 100L)))
  fs_param_grids <- list(
    "Lasso" = list(
      "feature_selector__estimator__C" = c(0.01, 0.1, 1L, 10L),
      "feature_selector__estimator__solver" = c('liblinear','saga')
    ),
    "Univariate" = list(
      "feature_selector__param" = seq(10L, max_features, by = step_size)
    ),
    "boruta" = list(
      "feature_selector__perc" = seq(80L, 100L, by = 10L),
      'feature_selector__n_estimators' = c(50L, 100L, 250L, 500L)
    ),
    "RandomForest" = list(
      "feature_selector__estimator__n_estimators" = seq(100L, 500L,by = 50L),
      "feature_selector__estimator__max_depth" = c(10L, 20L, 30L),
      "feature_selector__estimator__min_samples_split" = c(2L, 5L, 10L),
      "feature_selector__estimator__min_samples_leaf" = c(1L, 2L, 4L),
      "feature_selector__estimator__bootstrap" = c(TRUE, FALSE)
    ))
    classifier_param_grid <- list(
      "classifier__n_estimators" = seq(100L, 1000L, 400L),
      "classifier__max_depth" = c(10L, 20L, 30L, 40L, 50L),
      "classifier__min_samples_split" = c(2L, 5L, 10L),
      "classifier__min_samples_leaf" = c(1L, 2L, 4L),
      "classifier__max_features" = c('auto', 'sqrt')
      )

  return(list(fs_param_grids = fs_param_grids,
              classifier_param_grid = classifier_param_grid))
}

#' Split Data into Training and Test Sets
#'
#' @param X A dataframe or matrix of predictors.
#' @param y A vector of outcomes.
#' @param test_size Proportion of the data to be used as the test set.
#' @param modules A list containing the definitions for the Python modules and submodules.
#' @return A list containing the training and test sets for predictors and outcomes.
#' @export
split_data <- function(X, y, test_size, modules) {
  split_data <- modules$model_selection$train_test_split(X, y, test_size = test_size)
  list(
    X_train = reticulate::r_to_py(split_data[[1]]),
    X_test = reticulate::r_to_py(split_data[[2]]),
    y_train = reticulate::r_to_py(split_data[[3]]),
    y_test = reticulate::r_to_py(split_data[[4]])
  )
}

#' Perform Grid Search or Random Search for Hyperparameter Tuning
#'
#' @param X_train Training data for predictors.
#' @param y_train Training data for outcomes.
#' @param pipeline A pipeline specifying the steps for feature selection and model training.
#' @param scoring A string representing what scoring metric to use for hyperparameter adjustment. Default value is 'accuracy'
#' @param params A list of parameters or parameter distributions to search over.
#' @param search_type A character string specifying the type of search ('grid' or 'random').
#' @param n_iter The number of parameter settings that are sampled in a random search.
#' @param njobs The number of CPU cores to use.
#' @param modules A list containing the definitions for the Python modules and submodules.
#' @param random_state An integer value setting the random seed for feature selection algorithms and cross validation procedure. By default set to NULL to use different random seed every time an algorithm is used. For reproducibility could be fixed, otherwise for an unbiased estimation should be left as NULL.
#' @return An object of the optimal model found during the search.
#' @export
perform_grid_search <- function(X_train, y_train, pipeline, scoring, params, search_type, n_iter = NULL, njobs, modules, random_state = NULL) {
  if (search_type == 'grid') {
    search_cv <- modules$grid(
      estimator = pipeline,
      scoring = scoring,
      param_grid = params,
      cv = 5L,
      n_jobs = njobs,
      verbose = 1L
    )
  } else if (search_type == 'random') {
    if (is.null(n_iter)) stop("n_iter must be provided for random search")
    search_cv <- modules$randomized_grid(
      estimator = pipeline,
      scoring = scoring,
      param_distributions = params,
      cv = 5L,
      n_iter = n_iter,
      n_jobs = njobs,
      verbose = 1L,
      random_state = if (!is.null(random_state)) random_state else NULL
    )
  } else if (search_type == 'bayesian') {
    if (is.null(n_iter)) stop("n_iter must be provided for bayesian search")
    search_cv <- modules$bayesianCV(
      estimator = pipeline,
      scoring = scoring,
      search_spaces = params,
      cv = 5L,
      n_iter = n_iter,
      n_points = 5L,
      n_jobs = njobs,
      verbose = 1L
    )
  } else {
    stop("Invalid search_type. Choose either 'grid', 'random' or 'bayesian'.")
  }

  search_cv$fit(X_train, y_train)
  return(search_cv)
}


#' Convert Scikit-learn Pipeline to Named List
#'
#' This function takes a Scikit-learn Pipeline object and converts it to a named list in R.
#' Each step in the pipeline becomes an element in the list with the name of the step as the name of the list element.
#'
#' @param pipeline A Scikit-learn Pipeline object.
#'
#' @return A named list where each element represents a step in the pipeline.
#' The names of the list elements correspond to the names of the steps in the pipeline.
#' @importFrom reticulate py_to_r
#' @export
pipeline_to_list <- function(pipeline) {
  pipeline <- reticulate::py_to_r(pipeline)
  steps <- pipeline$steps
  named_list <- list()

  for (step in steps) {
    step_name <- step[[1]][[1]]
    if (!step_name %in% names(named_list)) {
      step_obj <- step[[2]]
      named_list[[step_name]] <- step_obj
    }
  }
  return(named_list)
}

#' Evaluate Test Metrics for a Grid Search Model
#'
#' This function takes a grid search object, test data, and test labels to evaluate the performance
#' of the best model found during grid search.
#'
#' @param grid_search A grid search object containing the best estimator.
#' @param X_test A data frame or matrix of test features.
#' @param y_test A vector of test labels.
#' @param modules A list of Python modules used in the function.
#'
#' @return A list containing the weighted precision, weighted recall, weighted F1 score, and accuracy.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming grid_search, X_test, y_test, and sklearn are defined
#' metrics <- evaluate_test_metrics(grid_search, X_test, y_test, sklearn)
#' }
#'@export
evaluate_test_metrics <- function(grid_search, X_test, y_test, modules) {
  best_model <- grid_search$best_estimator_
  y_pred <- best_model$predict(X_test)
  precision <- sklearn$metrics$precision_score(y_test, y_pred, average = "weighted")
  recall <- sklearn$metrics$recall_score(y_test, y_pred, average = "weighted")
  f1 <- sklearn$metrics$f1_score(y_test, y_pred, average = "weighted")
  accuracy <- sklearn$metrics$accuracy_score(y_test, y_pred)
  return(list(precision = precision, recall = recall, f1 = f1, accuracy = accuracy))
}

#' Create a Dataframe of Test Metrics
#'
#' @param test_metrics A list or dataframe of test metrics.
#' @return A dataframe with the processed test metrics.
#'
create_test_metrics_df <- function(test_metrics) {
  test_metrics_df <- test_metrics %>%
    tibble::enframe(name = "split", value = 'methods') %>%
    tidyr::unnest_longer(.data$methods, indices_to = 'method') %>%
    tidyr::unnest_wider(.data$methods)

  test_metrics_df <- test_metrics_df %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(dplyr::across(c(dplyr::starts_with("f1"), dplyr::starts_with("recall"),
                                     dplyr::starts_with("precision"), dplyr::starts_with("accuracy")),
                                   list(mean = mean, sd = sd), .names = "{.col}_{.fn}"))
  return(test_metrics_df)
}

#' Calculate Mean Cross-Validation Scores for Various Feature Selection Methods
#'
#' @param selected_pipelines A list of pipelines for different feature selection methods.
#' @param cv_best_score A list or vector of cross-validation scores.
#' @return A dataframe containing the mean and standard deviation of cross-validation scores for each method.
#'
calculate_mean_cv_scores <- function(selected_pipelines, cv_best_score) {
  overall_mean_cv_scores <- list()
  overall_sd_cv_scores <- list()

  for (method in names(selected_pipelines)) {
    method_test_scores <- sapply(cv_best_score, function(x) x[[method]])
    overall_mean_cv_scores[[method]] <- mean(method_test_scores)
    overall_sd_cv_scores[[method]] <- sd(method_test_scores)
  }

  mean_df <- data.frame(method = names(overall_mean_cv_scores),
                        mean_score = unlist(overall_mean_cv_scores),
                        stringsAsFactors = FALSE)

  sd_df <- data.frame(method = names(overall_sd_cv_scores),
                      sd_score = unlist(overall_sd_cv_scores),
                      stringsAsFactors = FALSE)

  return(merge(mean_df, sd_df, by = "method"))
}

#' Gene Selection and Evaluation with GeneSelectR
#'
#' @description This function performs gene selection using different methods on a given
#' training set and evaluates their performance using cross-validation. Optionally, it
#' also calculates permutation feature importances.
#'
#' @param X A matrix or data frame with features as columns and observations as rows.
#' @param y A vector of labels corresponding to the rows of X_train.
#' @param pipelines An optional list of pre-defined pipelines to use for fitting and evaluation. If this argument is provided, the feature selection methods and preprocessing steps will be ignored.
#' @param custom_fs_methods An optional list of feature selection methods to use for fitting and evaluation. If this argument is not provided, a default set of feature selection methods will be used.
#' @param selected_methods An optional vector of names of feature selection methods to use from the default set. If this argument is provided, only the specified methods will be used.
#' @param custom_fs_grids An optional list of hyperparameter grids for the feature selection methods. Each element of the list should be a named list of parameters for a specific feature selection method. The names of the elements should match the names of the feature selection methods. If this argument is provided, the function will perform hyperparameter tuning for the specified feature selection methods in addition to the final estimator.
#' @param classifier An optional sklearn classifier. If left NULL then sklearn RandomForestClassifier is used.
#' @param classifier_grid An optional named list of classifier parameters. If none are provided then default grid is used (check vignette for exact params).
#' @param preprocessing_steps An optional named list of sklearn preprocessing procedures. If none provided defaults are used (check vignette for exact params).
#' @param testsize The size of the test set used in the evaluation.
#' @param validsize The size of the validation set used in the evaluation.
#' @param scoring A string representing what scoring metric to use for hyperparameter adjustment. Default value is 'accuracy'
#' @param njobs Number of jobs to run in parallel.
#' @param n_splits Number of train/test splits.
#' @param search_type A string indicating the type of search to use. 'grid' for GridSearchCV and 'random' for RandomizedSearchCV. Default is 'random'.
#' @param n_iter An integer indicating the number of parameter settings that are sampled in RandomizedSearchCV. Only applies when search_type is 'random'.
#' @param calculate_permutation_importance A boolean indicating whether to calculate permutation feature importance. Default is FALSE.
#' @param max_features Maximum number of features to be selected by default feature selection methods.
#' @param perform_test_split Whether to perform train and test split, to have an evaluation on unseen test set. The default value is set to FALSE
#' @param random_state An integer value setting the random seed for feature selection algorithms and cross validation procedure. By default set to NULL to use different random seed every time an algorithm is used. For reproducibility could be fixed, otherwise for an unbiased estimation should be left as NULL.
#' @return A list with the following elements:
#' \item{fitted_pipelines}{A list of the fitted pipelines.}
#' \item{cv_results}{A list of the cross-validation results for each pipeline.}
#' \item{inbuilt_feature_importance}{A list of the inbuilt feature importance scores for each pipeline.}
#' \item{gene_set_stability}{A list of the gene set stability for each pipeline.}
#' \item{test_metrics}{A data frame of test metrics for each pipeline.}
#' \item{permutation_importances}{A list of the permutation importance scores for each pipeline (if calculate_permutation_importance is TRUE).}
#' @examples
#' \dontrun{
#' # Perform gene selection and evaluation using the default methods
#' data(iris)
#' X <- iris[,1:4]
#' y <- iris[,5]
#' results <- GeneSelectR(X_train = X, y_train = y)
#'
#' # Perform gene selection and evaluation using a subset of the default methods
#' results <- GeneSelectR(X_train = X, y_train = y, selected_methods = c("Univariate", "RFE"))
#'
#' # Perform gene selection and evaluation using user-defined methods
#' fs_methods <- list("Lasso" = select_model(lasso(penalty = 'l1',
#'                                                 C = 0.1,
#'                                                 solver = 'saga'),
#'                                           threshold = 'median'))
#' custom_fs_grids <- list("Lasso" = list('C' = c(0.1, 1, 10)))
#' results <- GeneSelectR(X_train = X,
#'                        y_train = y,
#'                        custom_fs_methods = fs_methods,
#'                        custom_fs_grids = custom_fs_grids)
#'}
#' @importFrom reticulate import r_to_py
#' @importFrom glue glue
#' @importFrom dplyr bind_rows group_by summarise across starts_with
#' @importFrom tibble enframe
#' @importFrom tidyr unnest_longer unnest_wider
#' @importFrom stats setNames
#' @importFrom magrittr '%>%'
#' @importFrom methods new
#' @importFrom rlang .data
#' @export
#'
GeneSelectR <- function(X,
                        y,
                        pipelines = NULL,
                        custom_fs_methods = NULL,
                        selected_methods = NULL,
                        custom_fs_grids = NULL,
                        classifier = NULL,
                        classifier_grid = NULL,
                        preprocessing_steps = NULL,
                        testsize = 0.2,
                        validsize = 0.2,
                        scoring = 'accuracy',
                        njobs = -1,
                        n_splits = 5,
                        search_type = 'random',
                        n_iter = 10,
                        max_features = 50,
                        calculate_permutation_importance = FALSE,
                        perform_test_split = FALSE,
                        random_state = NULL) {
  # Check if the number of features is less than 50
  if (ncol(X) < 50) {
    stop("The number of features in the input data must be at least 50. Please rerun without 'Univariate' feature selection method.")
  }

  message('Performing feature selection procedure. Please wait, it takes some time')

  # Convert certain parameters to integers
  njobs <- as.integer(njobs)
  n_splits <- as.integer(n_splits)
  n_iter <- as.integer(n_iter)
  max_features <- as.integer(max_features)
  if (!is.null(random_state)) {
    random_state <- as.integer(random_state)
  }

  if (Sys.info()["sysname"] == "Windows") {
    enable_multiprocess()
  }

  modules <- define_sklearn_modules()
  default_feature_selection_methods <- set_default_fs_methods(modules, max_features,random_state = random_state)
  default_classifier <- modules$forest(random_state = random_state)
  default_grids <- set_default_param_grids(max_features)

  # define feature selection grids, set to default if none are provided
  if (is.null(custom_fs_grids)) {
    fs_grids <- default_grids$fs_param_grids
  } else fs_grids <- custom_fs_grids

  # Use the default feature selection methods if none are provided
  if (is.null(custom_fs_methods)) {
    fs_methods <- default_feature_selection_methods$default_feature_selection_methods
  } else fs_methods <- custom_fs_methods

  if (is.null(preprocessing_steps)) {
    preprocessing_steps <- default_feature_selection_methods$preprocessing_steps
  } else preprocessing_steps <- preprocessing_steps

  # Use default classifier if one is not provided
  if (is.null(classifier)) {
    classifier <- default_classifier
  } else classifier <- classifier

  # Use default classifier grid if none is provided
  if (is.null(classifier_grid)) {
    classifier_params <- default_grids$classifier_param_grid
  } else classifier_params <- classifier_grid

  # Select the specified feature selection methods if they are provided
  if (!is.null(selected_methods)) {
    fs_methods <- fs_methods[selected_methods]
  }

  # Select the specified pipelines if they are provided
  if (!is.null(pipelines)) {
    selected_pipelines <- pipelines
  } else {
    # Define the default pipelines using the selected feature selection methods
    selected_pipelines <- create_pipelines(fs_methods,
                                           preprocessing_steps,
                                           fs_param_grids = fs_grids,
                                           classifier = classifier)
  }

  # convert R objects to Py
  X_train <- reticulate::r_to_py(X)
  y_train <- reticulate::r_to_py(y)
  #y_train <- y_train$values$ravel()

  fitted_pipelines <- list()
  cv_results <- list()
  selected_features <- list()
  split_results <- list()
  cv_best_score <- list()
  sd_performances <- list()
  test_metrics <- list()
  permutation_importances <- list()

  # Repeated train-test splitting
  for (split_idx in 1:n_splits) {
    message(glue::glue("Fitting the data split: {split_idx} \n"))

    if (perform_test_split) {
      split_results <- split_data(X_train, y_train, testsize, modules)
      X_train_split <- split_results$X_train
      X_test_split <- split_results$X_test
      y_train_split <- split_results$y_train
      y_test_split <- split_results$y_test
    } else {
      X_train_split <- X_train
      y_train_split <- y_train
    }

    split_fitted_pipelines <- list()
    split_cv_results <- list()
    split_best_score <- list()
    split_selected_features <- list()
    split_mean_performances <- list()
    split_test_metrics <- list()
    split_permutation_importances <- list()

    for (i in seq_along(names(selected_pipelines))) {

      message(glue("Fitting pipeline for {names(selected_pipelines)[[i]]} feature selection method\n"))

      # Split training data further into training and validation sets
      train_valid_split <- split_data(X_train_split, y_train_split, test_size = validsize, modules)
      X_train_sub_split <- train_valid_split$X_train
      X_valid_split <- train_valid_split$X_test
      y_train_sub_split <- train_valid_split$y_train
      y_valid_split <- train_valid_split$y_test


      fs_name <- names(selected_pipelines)[i]
      fs_params <- if (!is.null(custom_fs_grids) && fs_name %in% names(custom_fs_grids)) {
        custom_fs_grids[[fs_name]]
      } else if (fs_name %in% names(fs_grids)) {
        fs_grids[[fs_name]]
      } else {
        NULL
      }

      params <- c(classifier_params, fs_params)

      print(classifier)
      print(classifier_params)

      search_cv <- perform_grid_search(
        X_train_sub_split,
        y_train_sub_split,
        selected_pipelines[[i]],
        scoring,
        params,
        search_type,
        n_iter,
        njobs,
        modules,
        random_state = random_state
      )

      best_model <- search_cv$best_estimator_


      if (perform_test_split) {
        test_set_metrics <- evaluate_test_metrics(search_cv, X_test = X_test_split, y_test = y_test_split, modules)
        split_test_metrics[[names(selected_pipelines)[[i]]]] <- test_set_metrics

      }

      # Save the fitted pipeline and results for the current split
      split_fitted_pipelines[[names(selected_pipelines)[[i]]]] <- pipeline_to_list(best_model)
      split_cv_results[[names(selected_pipelines)[[i]]]] <- search_cv$cv_results_
      split_best_score[[names(selected_pipelines)[[i]]]] <- search_cv$best_score_
      split_selected_features[[names(selected_pipelines)[[i]]]] <- get_feature_importances(best_model, X_train_sub_split, pipeline_name = names(selected_pipelines)[[i]], iter = split_idx)

      if (calculate_permutation_importance) {
        message('Performing Permuation Importance Calculation')
        split_permutation_importances[[names(selected_pipelines)[[i]]]] <-
          calculate_permutation_feature_importance(best_model, X_valid_split, y_valid_split, pipeline_name = names(selected_pipelines)[[i]], iter = split_idx, njobs = njobs)
      }
    }

    # Save the mean and sd of test scores for this split
    cv_best_score[[glue::glue('split_{split_idx}')]] <- split_best_score

    # Append the results of the current split to the main results list
    split_results[[glue::glue('split_{split_idx}')]] <- split_fitted_pipelines
    selected_features[[glue::glue('split_{split_idx}')]] <- split_selected_features
    cv_results[[glue::glue('split_{split_idx}')]] <- split_cv_results
    test_metrics[[glue::glue('split_{split_idx}')]] <- split_test_metrics
    permutation_importances[[glue::glue('split_{split_idx}')]] <- split_permutation_importances
  }

  if (perform_test_split) {
    test_metrics_df <- create_test_metrics_df(test_metrics)
  }


  # Calculate the mean feature importance for each method across all splits
  inbuilt_feature_importance <- aggregate_feature_importances(selected_features)
  # Calculate the mean and standard deviation of the permutation importances for each feature across all splits
  if (calculate_permutation_importance) {
    mean_permutation_importances <- aggregate_feature_importances(permutation_importances)
  }

  # Merge mean_df and sd_df into a single data frame
  cv_score_summary_df <- calculate_mean_cv_scores(selected_pipelines, cv_best_score)


  return(methods::new("PipelineResults",
                      best_pipeline = split_results,
                      cv_results = cv_results,
                      inbuilt_feature_importance = inbuilt_feature_importance,
                      test_metrics = if (perform_test_split) test_metrics_df else list(),
                      cv_mean_score = cv_score_summary_df,
                      permutation_importance = if (calculate_permutation_importance) mean_permutation_importances else list()))
}


