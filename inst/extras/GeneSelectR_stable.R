#' Gene Selection and Evaluation with GeneSelectR
#'
#' @description This function performs gene selection using different methods on a given
#' training set and evaluates their performance using cross-validation. Optionally, it
#' also calculates permutation feature importances.
#'
#' @param X_train A matrix or data frame of training data with features as columns and observations as rows.
#' @param y_train A vector of training labels corresponding to the rows of X_train.
#' @param pipelines An optional list of pre-defined pipelines to use for fitting and evaluation. If this argument is provided, the feature selection methods and preprocessing steps will be ignored.
#' @param feature_selection_methods An optional list of feature selection methods to use for fitting and evaluation. If this argument is not provided, a default set of feature selection methods will be used.
#' @param selected_methods An optional vector of names of feature selection methods to use from the default set. If this argument is provided, only the specified methods will be used.
#' @param fs_param_grids An optional list of hyperparameter grids for the feature selection methods. Each element of the list should be a named list of parameters for a specific feature selection method. The names of the elements should match the names of the feature selection methods. If this argument is provided, the function will perform hyperparameter tuning for the specified feature selection methods in addition to the final estimator.
#' @param testsize The size of the test set used in the evaluation.
#' @param validsize The size of the validation set used in the evaluation.
#' @param njobs Number of jobs to run in parallel.
#' @param n_splits Number of train/test splits.
#' @param search_type A string indicating the type of search to use. 'grid' for GridSearchCV and 'random' for RandomizedSearchCV. Default is 'random'.
#' @param n_iter An integer indicating the number of parameter settings that are sampled in RandomizedSearchCV. Only applies when search_type is 'random'.
#' @param calculate_permutation_importance A boolean indicating whether to calculate permutation feature importance. Default is FALSE.
#' @param max_features Maximum number of features to be selected by default feature selection methods
#' @param perform_split Whether to perform train and test split, to have an evaluation on unseen test set. The default value is set to TRUE
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
#' fs_param_grids <- list("Lasso" = list('C' = c(0.1, 1, 10)))
#' results <- GeneSelectR(X_train = X,
#'                        y_train = y,
#'                        feature_selection_methods = fs_methods,
#'                        fs_param_grids = fs_param_grids)
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
#' @importFrom utils globalVariables
#' @export
#'
GeneSelectR <- function(X_train,
                        y_train,
                        pipelines = NULL,
                        feature_selection_methods = NULL,
                        selected_methods = NULL,
                        fs_param_grids = NULL,
                        testsize = 0.2,
                        validsize = 0.2,
                        njobs = -1L,
                        n_splits = 2L,
                        search_type = 'random',
                        n_iter = 10L,
                        max_features = 50L,
                        calculate_permutation_importance = FALSE,
                        perform_split = TRUE) {

  message('Performing feature selection procedure. Please wait, it takes some time')

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
  randomized_grid <- sklearn$model_selection$RandomizedSearchCV
  grid <- sklearn$model_selection$GridSearchCV
  lasso <- sklearn$linear_model$LogisticRegression
  univariate <- sklearn$feature_selection$GenericUnivariateSelect
  select_model <- sklearn$feature_selection$SelectFromModel

  # define a classifier for the accuracy estimation
  GradBoost <- ensemble$GradientBoostingClassifier

  # Define preprocessing steps as a list
  preprocessing_steps <- list(
    "VarianceThreshold" = sklearn$feature_selection$VarianceThreshold(0.85),
    "MinMaxScaler" = sklearn$preprocessing$MinMaxScaler()
  )

  # Create a list of default feature selection methods
  default_feature_selection_methods <- list(
    "Lasso" = select_model(lasso(penalty = 'l1', C = 0.1, solver = 'saga'), threshold = 'median', max_features = max_features),
    'Univariate' = univariate(mode = 'k_best',param = 200L),
    'RandomForest' = select_model(forest(n_estimators=100L, random_state=42L), threshold = 'median', max_features = max_features),
    'boruta'= boruta$BorutaPy(forest(), n_estimators = 'auto', verbose = 0,perc = 100L)
  )

  # Define default parameter grids if none are provided
  if (is.null(fs_param_grids)) {
    fs_param_grids <- list(
      "Lasso" = list(
        "feature_selector__estimator__C" = c(0.01, 0.1, 1L, 10L),
        "feature_selector__estimator__solver" = c('liblinear','saga')
      ),
      "Univariate" = list(
        "feature_selector__param" = seq(50L, 200L, by = 50L)
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
      )
    )
  }

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
                                           fs_param_grids = fs_param_grids,
                                           classifier = forest())
  }

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
    if (perform_split) {
      split_data <- model_selection$train_test_split(X_train, y_train, test_size = testsize)
      X_train_split <- reticulate::r_to_py(split_data[[1]])
      X_test_split <- reticulate::r_to_py(split_data[[2]])
      y_train_split <- reticulate::r_to_py(split_data[[3]])
      y_test_split <- reticulate::r_to_py(split_data[[4]])
    } else {
      X_train_split <- X_train
      X_test_split <- NULL
      y_train_split <- y_train
      y_test_split <- NULL
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
      train_valid_split <- model_selection$train_test_split(X_train_split, y_train_split, test_size = validsize)
      X_train_sub_split <- reticulate::r_to_py(train_valid_split[[1]])
      X_valid_split <- reticulate::r_to_py(train_valid_split[[2]])
      y_train_sub_split <- reticulate::r_to_py(train_valid_split[[3]])
      y_valid_split <- reticulate::r_to_py(train_valid_split[[4]])

      # Create the parameter grid with the 'classifier' prefix
      params <- stats::setNames(
        list(
          seq(100L, 1000L, 400L), # n_estimators
          c(10L, 20L, 30L, 40L, 50L), # max_depth
          c(2L, 5L, 10L), # min_samples_split
          c(1L, 2L, 4L), # min_samples_leaf
          c('auto', 'sqrt') # max_features
        ),
        c(
          "classifier__n_estimators",
          "classifier__max_depth",
          "classifier__min_samples_split",
          "classifier__min_samples_leaf",
          "classifier__max_features"
        )
      )

      if (!is.null(fs_param_grids)) {
        fs_name <- names(selected_pipelines)[i]
        if (fs_name %in% names(fs_param_grids)) {
          fs_params <- fs_param_grids[[fs_name]]
          params <- c(params, fs_params)
        }
      }

      if (search_type == 'grid') {
        search_cv <- grid(
          estimator = selected_pipelines[[i]],
          param_grid = params,
          cv=5L,
          n_jobs = njobs,
          verbose = 2L)
      } else if (search_type == 'random') {
        search_cv <- randomized_grid(
          estimator = selected_pipelines[[i]],
          param_distributions = params,
          cv=5L,
          n_iter = n_iter,
          n_jobs = njobs,
          verbose = 1L)
      } else {
        stop("Invalid search_type. Choose either 'grid' or 'random'.")
      }

      search_cv$fit(X_train_sub_split, y_train_sub_split)

      if (perform_split) {
        # Evaluate the best model on the test set
        best_model <- search_cv$best_estimator_
        y_pred <- best_model$predict(X_test_split)
        precision <- sklearn$metrics$precision_score(y_test_split, y_pred, average = "weighted")
        recall <- sklearn$metrics$recall_score(y_test_split, y_pred, average = "weighted")
        f1 <- sklearn$metrics$f1_score(y_test_split, y_pred, average = "weighted")
        accuracy <- sklearn$metrics$accuracy_score(y_test_split, y_pred)
        split_test_metrics[[names(selected_pipelines)[[i]]]] <- list(precision = precision, recall = recall, f1 = f1, accuracy = accuracy) # save the other metrics for the current split

      }

      # Save the fitted pipeline and results for the current split
      split_fitted_pipelines[[names(selected_pipelines)[[i]]]] <- search_cv$best_estimator_
      split_cv_results[[names(selected_pipelines)[[i]]]] <- search_cv$cv_results_
      split_best_score[[names(selected_pipelines)[[i]]]] <- search_cv$best_score_
      split_selected_features[[names(selected_pipelines)[[i]]]] <- get_feature_importances(split_fitted_pipelines[[i]], X_train_sub_split, pipeline_name = names(selected_pipelines)[[i]], iter = split_idx)

      if (calculate_permutation_importance) {
        message('Performing Permuation Importance Calculation')
        split_permutation_importances[[names(selected_pipelines)[[i]]]] <-
          calculate_permutation_feature_importance(best_model, X_valid_split, y_valid_split, pipeline_name = names(selected_pipelines)[[i]], iter = split_idx)
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

  if (perform_split) {
    utils::globalVariables(c("split", "methods", "method"))
    test_metrics_df <- test_metrics %>%
      tibble::enframe(name = "split", value = 'methods') %>%
      tidyr::unnest_longer(methods, indices_to = 'method') %>%
      tidyr::unnest_wider(methods)

    test_metrics_df <- test_metrics_df %>%
      dplyr::group_by(method) %>%
      dplyr::summarise(dplyr::across(c(dplyr::starts_with("f1"), dplyr::starts_with("recall"),
                                       dplyr::starts_with("precision"), dplyr::starts_with("accuracy")),
                                     list(mean = mean, sd = sd), .names = "{.col}_{.fn}"))
  }


  # Calculate the mean feature importance for each method across all splits
  inbuilt_feature_importance <- aggregate_feature_importances(selected_features)
  # Calculate the mean and standard deviation of the permutation importances for each feature across all splits
  if (calculate_permutation_importance) {
    mean_permutation_importances <- aggregate_feature_importances(permutation_importances)
  }

  # Calculate the overall mean and standard deviation of the mean test scores for each method
  overall_mean_cv_scores <- list()
  overall_sd_cv_scores <- list()

  # Loop over each feature selection method
  for (method in names(selected_pipelines)) {
    # Extract the mean test scores for this method across all splits
    method_test_scores <- sapply(cv_best_score, function(x) x[[method]])

    # Calculate the mean and sd of the mean test scores for this method
    overall_mean_cv_scores[[method]] <- mean(method_test_scores)
    overall_sd_cv_scores[[method]] <- sd(method_test_scores)
  }

  # Convert the overall mean and sd test scores into data frames
  mean_df <- data.frame(method = names(overall_mean_cv_scores), mean_score = unlist(overall_mean_cv_scores), stringsAsFactors = FALSE)
  sd_df <- data.frame(method = names(overall_sd_cv_scores), sd_score = unlist(overall_sd_cv_scores), stringsAsFactors = FALSE)

  # Merge mean_df and sd_df into a single data frame
  cv_score_summary_df <- merge(mean_df, sd_df, by = "method")


  return(methods::new("PipelineResults",
                      fitted_pipelines = split_results,
                      cv_results = cv_results,
                      inbuilt_feature_importance = inbuilt_feature_importance,
                      test_metrics = if (perform_split) test_metrics_df else list(),
                      cv_mean_score = cv_score_summary_df,
                      permutation_importances = if (calculate_permutation_importance) mean_permutation_importances else list()))
}
