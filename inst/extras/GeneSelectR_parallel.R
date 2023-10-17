#' #' Process a single data split for model training and evaluation
#' #'
#' #' This function performs various tasks for a given data split index:
#' #' - Splits the data into training and test sets if required
#' #' - Trains machine learning pipelines specified in global variables
#' #' - Evaluates the trained models
#' #' - Computes feature importances
#' #' - Optionally computes permutation importances
#' #'
# #' @param split_idx The index of the data split to process.
# #' @return A list containing various results including best models, evaluation metrics, and feature importances.
# #' @export
#' process_split <- function(split_idx,
#'                           perform_test_split,
#'                           X_train,
#'                           y_train,
#'                           selected_pipelines,
#'                           testsize,
#'                           validsize,
#'                           custom_fs_grids,
#'                           fs_grids,
#'                           classifier_params,
#'                           njobs,
#'                           scoring,
#'                           search_type,
#'                           n_iter,
#'                           calculate_permutation_importance) {
#'   message(glue::glue("Fitting the data split: {split_idx}"))
#'   # GeneSelectR::set_reticulate_python()
#'   modules <- GeneSelectR::define_sklearn_modules()
#'
#'   if (perform_test_split) {
#'     split_results <- GeneSelectR::split_data(X_train, y_train, test_size = testsize, modules = modules)
#'   } else {
#'     split_results <- list(X_train = X_train, y_train = y_train)
#'   }
#'
#'   X_train_split <- split_results$X_train
#'   y_train_split <- split_results$y_train
#'
#'   split_fitted_pipelines <- list()
#'   split_cv_results <- list()
#'   split_best_score <- list()
#'   split_selected_features <- list()
#'   split_test_metrics <- list()
#'   split_permutation_importances <- list()
#'
#'   pipeline_names <- names(selected_pipelines)
#'
#'   for (i in seq_along(pipeline_names)) {
#'     fs_name <- pipeline_names[i]
#'     message(glue("Fitting pipeline for {fs_name} feature selection method"))
#'
#'     train_valid_split <- GeneSelectR::split_data(X_train_split, y_train_split, test_size = validsize, modules = modules)
#'
#'     fs_params <- if (!is.null(custom_fs_grids) && fs_name %in% names(custom_fs_grids)) {
#'       custom_fs_grids[[fs_name]]
#'     } else if (fs_name %in% names(fs_grids)) {
#'       fs_grids[[fs_name]]
#'     } else {
#'       NULL
#'     }
#'
#'     params <- c(classifier_params, fs_params)
#'
#'     search_cv <- GeneSelectR::perform_grid_search(
#'       train_valid_split$X_train,
#'       train_valid_split$y_train,
#'       selected_pipelines[[i]],
#'       scoring,
#'       params,
#'       search_type,
#'       n_iter,
#'       njobs,
#'       modules = modules
#'     )
#'
#'     best_model <- search_cv$best_estimator_
#'
#'     if (perform_test_split) {
#'       test_set_metrics <- GeneSelectR::evaluate_test_metrics(search_cv, X_test = split_results$X_test, y_test = split_results$y_test, modules = modules)
#'       split_test_metrics[[fs_name]] <- test_set_metrics
#'     }
#'     split_fitted_pipelines[[fs_name]] <- GeneSelectR::pipeline_to_list(best_model)
#'     split_cv_results[[fs_name]] <- search_cv$cv_results_
#'     split_best_score[[fs_name]] <- search_cv$best_score_
#'     split_selected_features[[fs_name]] <- GeneSelectR::get_feature_importances(best_model, train_valid_split$X_train, pipeline_name = fs_name, iter = split_idx)
#'
#'     if (calculate_permutation_importance) {
#'       message('Performing Permuation Importance Calculation')
#'       split_permutation_importances[[fs_name]] <- GeneSelectR::calculate_permutation_feature_importance(best_model, train_valid_split$X_test, train_valid_split$y_test, pipeline_name = fs_name, iter = split_idx, njobs = njobs)
#    }
#   }
#'
#'   return(list(
#'     cv_best_score = split_best_score,
#'     split_results = split_fitted_pipelines,
#'     selected_features = split_selected_features,
#'     cv_results = split_cv_results,
#'     test_metrics = split_test_metrics,
#'     permutation_importances = split_permutation_importances
#  ))
# }
#'
#' #' Gene Selection and Evaluation with GeneSelectR
#' #'
# #' @description This function performs gene selection using different methods on a given
#' #' training set and evaluates their performance using cross-validation. Optionally, it
#' #' also calculates permutation feature importances.
#' #'
# #' @param X A matrix or data frame with features as columns and observations as rows.
# #' @param y A vector of labels corresponding to the rows of X_train.
# #' @param pipelines An optional list of pre-defined pipelines to use for fitting and evaluation. If this argument is provided, the feature selection methods and preprocessing steps will be ignored.
# #' @param custom_fs_methods An optional list of feature selection methods to use for fitting and evaluation. If this argument is not provided, a default set of feature selection methods will be used.
# #' @param selected_methods An optional vector of names of feature selection methods to use from the default set. If this argument is provided, only the specified methods will be used.
# #' @param custom_fs_grids An optional list of hyperparameter grids for the feature selection methods. Each element of the list should be a named list of parameters for a specific feature selection method. The names of the elements should match the names of the feature selection methods. If this argument is provided, the function will perform hyperparameter tuning for the specified feature selection methods in addition to the final estimator.
# #' @param classifier An optional sklearn classifier. If left NULL then sklearn RandomForestClassifier is used.
# #' @param classifier_grid An optional named list of classifier parameters. If none are provided then default grid is used (check vignette for exact params).
# #' @param preprocessing_steps An optional named list of sklearn preprocessing procedures. If none provided defaults are used (check vignette for exact params).
# #' @param testsize The size of the test set used in the evaluation.
# #' @param validsize The size of the validation set used in the evaluation.
# #' @param scoring A string representing what scoring metric to use for hyperparameter adjustment. Default value is 'accuracy'
# #' @param njobs Number of jobs to run in parallel.
# #' @param n_splits Number of train/test splits.
# #' @param search_type A string indicating the type of search to use. 'grid' for GridSearchCV and 'random' for RandomizedSearchCV. Default is 'random'.
# #' @param n_iter An integer indicating the number of parameter settings that are sampled in RandomizedSearchCV. Only applies when search_type is 'random'.
# #' @param calculate_permutation_importance A boolean indicating whether to calculate permutation feature importance. Default is FALSE.
# #' @param max_features Maximum number of features to be selected by default feature selection methods.
# #' @param perform_test_split Whether to perform train and test split, to have an evaluation on unseen test set. The default value is set to FALSE
# #' @return A list with the following elements:
# #' \item{fitted_pipelines}{A list of the fitted pipelines.}
# #' \item{cv_results}{A list of the cross-validation results for each pipeline.}
# #' \item{inbuilt_feature_importance}{A list of the inbuilt feature importance scores for each pipeline.}
# #' \item{gene_set_stability}{A list of the gene set stability for each pipeline.}
# #' \item{test_metrics}{A data frame of test metrics for each pipeline.}
# #' \item{permutation_importances}{A list of the permutation importance scores for each pipeline (if calculate_permutation_importance is TRUE).}
# #' @examples
# \dontrun{
# #' # Perform gene selection and evaluation using the default methods
# #' data(iris)
# #' X <- iris[,1:4]
# #' y <- iris[,5]
# #' results <- GeneSelectR(X_train = X, y_train = y)
# #'
# #' # Perform gene selection and evaluation using a subset of the default methods
# #' results <- GeneSelectR(X_train = X, y_train = y, selected_methods = c("Univariate", "RFE"))
# #'
# #' # Perform gene selection and evaluation using user-defined methods
# #' fs_methods <- list("Lasso" = select_model(lasso(penalty = 'l1',
# #'                                                 C = 0.1,
# #'                                                 solver = 'saga'),
# #'                                           threshold = 'median'))
# #' custom_fs_grids <- list("Lasso" = list('C' = c(0.1, 1, 10)))
# #' results <- GeneSelectR(X_train = X,
# #'                        y_train = y,
# #'                        custom_fs_methods = fs_methods,
# #'                        custom_fs_grids = custom_fs_grids)
# #}
# # @importFrom reticulate import r_to_py
# # @importFrom glue glue
# # @importFrom dplyr bind_rows group_by summarise across starts_with
# # @importFrom tibble enframe
# # @importFrom tidyr unnest_longer unnest_wider
# # @importFrom stats setNames
# # @importFrom magrittr '%>%'
# # @importFrom methods new
# # @importFrom rlang .data
# # @importFrom  function
# # @export
# #
#' GeneSelectR_parallel <- function(X,
#'                         y,
#'                         pipelines = NULL,
#'                         custom_fs_methods = NULL,
#'                         selected_methods = NULL,
#'                         custom_fs_grids = NULL,
#'                         classifier = NULL,
#'                         classifier_grid = NULL,
#'                         preprocessing_steps = NULL,
#'                         testsize = 0.2,
#'                         validsize = 0.2,
#'                         scoring = 'accuracy',
#'                         njobs = -1,
#'                         n_splits = 5,
#'                         search_type = 'random',
#'                         n_iter = 10,
#'                         max_features = 50,
#'                         calculate_permutation_importance = FALSE,
#'                         perform_test_split = FALSE) {
#'
#'   message('Performing feature selection procedure. Please wait, it takes some time')
#'
#'   # python_file_path <- system.file("python", "GeneSelectR.py", package = "GeneSelectR")
#'   # reticulate::source_python(python_file_path)
#'
#'   # Convert certain parameters to integers
#'   njobs <- as.integer(njobs)
#'   n_splits <- as.integer(n_splits)
#'   n_iter <- as.integer(n_iter)
#'   max_features <- as.integer(max_features)
#'
#'   if (Sys.info()["sysname"] == "Windows") {
#'     enable_multiprocess()
#'   }
#'
#'   modules <- define_sklearn_modules()
#'   default_feature_selection_methods <- set_default_fs_methods(modules, max_features)
#'   default_classifier <- modules$forest()
#'   default_grids <- set_default_param_grids(max_features)
#'
#'   # define feature selection grids, set to default if none are provided
#'   if (is.null(custom_fs_grids)) {
#'     fs_grids <- default_grids$fs_param_grids
#'   } else fs_grids <- custom_fs_grids
#'
#'   # Use the default feature selection methods if none are provided
#'   if (is.null(custom_fs_methods)) {
#'     fs_methods <- default_feature_selection_methods$default_feature_selection_methods
#'   } else fs_methods <- custom_fs_methods
#'
#'   if (is.null(preprocessing_steps)) {
#'     preprocessing_steps <- default_feature_selection_methods$preprocessing_steps
#'   } else preprocessing_steps <- preprocessing_steps
#'
#'   # Use default classifier if one is not provided
#'   if (is.null(classifier)) {
#'     classifier <- default_classifier
#'   } else classifier <- classifier
#'
#'   # Use default classifier grid if none is provided
#'   if (is.null(classifier_grid)) {
#'     classifier_params <- default_grids$classifier_param_grid
#'   } else classifier_params <- classifier_grid
#'
#'   # Select the specified feature selection methods if they are provided
#'   if (!is.null(selected_methods)) {
#'     fs_methods <- fs_methods[selected_methods]
#'   }
#'
#'   # Select the specified pipelines if they are provided
#'   if (!is.null(pipelines)) {
#'     selected_pipelines <- pipelines
#'   } else {
#'     # Define the default pipelines using the selected feature selection methods
#'     selected_pipelines <- create_pipelines(fs_methods,
#'                                            preprocessing_steps,
#'                                            fs_param_grids = fs_grids,
#'                                            classifier = classifier)
#'   }
#'
#'   # convert R objects to Py
#'   X_train <- reticulate::r_to_py(X)
#'   y_train <- reticulate::r_to_py(y)
#'   y_train <- y_train$values$ravel()
#'
#'   fitted_pipelines <- list()
#'   cv_results <- list()
#'   selected_features <- list()
#'   split_results <- list()
#'   cv_best_score <- list()
#'   sd_performances <- list()
#'   test_metrics <- list()
#'   permutation_importances <- list()
#'
#'   # Setup for parallelization
#'   total_cores <- parallel::detectCores()
#'
#'   # If njobs is -1, use all available cores; otherwise, use the minimum of njobs and total_cores
#'   if (is.integer(njobs) && njobs == -1) {
#'     cores_for_outer_loop <- floor((total_cores - 1) / 2)
#'   } else if (is.integer(njobs) && njobs > 0) {
#'     cores_for_outer_loop <- floor(min(njobs, total_cores - 1) / 2)
#'   } else {
#'     cores_for_outer_loop <- 1  # Fallback to 1 core if njobs is not properly set
#'   }
#'
#'   # Ensure total_cores is an integer
#'   if (!is.integer(total_cores)) {
#'     total_cores <- as.integer(total_cores)
#'   }
#'
#'   # Calculate remaining cores for grid search
#'   cores_for_grid_search <- total_cores - 1 - cores_for_outer_loop
#'
#'
#'
#'   # Ensure cores_for_grid_search is an integer
#'   if (!is.integer(cores_for_grid_search)) {
#'     cores_for_grid_search <- as.integer(cores_for_grid_search)
#'   }
#'
#'   if (!is.integer(cores_for_outer_loop)) {
#'     cores_for_outer_loop<- as.integer(cores_for_outer_loop)
#'   }
#'
#'   # results <- run_pipelines(X_train,
#'   #                          y_train,
#'   #                          selected_pipelines = selected_pipelines,
#'   #                          testsize = testsize,
#'   #                          validsize = validsize,
#'   #                          custom_fs_grids = custom_fs_grids,
#'   #                          fs_grids = fs_grids,
#'   #                          classifier_params = classifier_params,
#'   #                          scoring = scoring,
#'   #                          search_type = search_type,
#'   #                          n_iter = n_iter,
#'   #                          n_splits = n_splits,
#'   #                          n_jobs = cores_for_outer_loop,
#'   #                          n_jobs_grid = cores_for_grid_search,
#'   #                          perform_test_split = perform_test_split,
#'   #                          calculate_permutation_importance = calculate_permutation_importance)
#'   #
#'   # print(results)
#'
#'   # Use lapply to run the loop serially
#'   all_results <- lapply(seq_len(n_splits), function(x) {
#'     process_split(
#'       x,
#'       selected_pipelines = selected_pipelines,
#'       X_train = X_train,
#'       y_train = y_train,
#'       testsize = testsize,
#'       validsize = validsize,
#'       custom_fs_grids = custom_fs_grids,
#'       fs_grids = fs_grids,
#'       classifier_params = classifier_params,
#'       scoring = scoring,
#'       search_type = search_type,
#'       n_iter = n_iter,
#'       njobs = njobs,
#'       perform_test_split = perform_test_split,
#'       calculate_permutation_importance = calculate_permutation_importance
#'
#'     )
#'   })
#'
#'   # # Register the parallel backend
#'   # cl <- parallel::makeCluster(cores_for_outer_loop)
#'   # doParallel::registerDoParallel(cl)
#'   # `%dorng%` <- doRNG::`%dorng%`
#'   #
#'   # # Parallel loop using foreach
#'   # all_results <- foreach::foreach(i=seq_len(n_splits), .packages=c("reticulate",'GeneSelectR')) %dorng% {
#'   #   GeneSelectR::set_reticulate_python()
#'   #   process_split(  # Replace with your actual Python function call
#'   #     i,
#'   #     selected_pipelines = selected_pipelines,
#'   #     X_train = X_train,
#'   #     y_train = y_train,
#'   #     testsize = testsize,
#'   #     validsize = validsize,
#'   #     custom_fs_grids = custom_fs_grids,
#'   #     fs_grids = fs_grids,
#'   #     classifier_params = classifier_params,
#'   #     scoring = scoring,
#'   #     search_type = search_type,
#'   #     n_iter = n_iter,
#'   #     njobs = cores_for_grid_search,
#'   #     perform_test_split = perform_test_split,
#'   #     calculate_permutation_importance = calculate_permutation_importance
#'   #   )
#'   # }
#'   #
#'   # # Stop the cluster
#'   # doParallel::stopCluster(cl)
#'
#'
#'   if (perform_test_split) {
#'     test_metrics_df <- create_test_metrics_df(all_results$test_metrics)
#'   }
#'
#'
#'   # Calculate the mean feature importance for each method across all splits
#'   inbuilt_feature_importance <- aggregate_feature_importances(all_results$selected_features)
#'   # Calculate the mean and standard deviation of the permutation importances for each feature across all splits
#'   if (calculate_permutation_importance) {
#'     mean_permutation_importances <- aggregate_feature_importances(all_results$permutation_importances)
#'   }
#'
#'   # Merge mean_df and sd_df into a single data frame
#'   cv_score_summary_df <- calculate_mean_cv_scores(selected_pipelines, all_results$cv_best_score)
#'
#'
#'   return(methods::new("PipelineResults",
#'                       best_pipeline = all_results$split_results,
#'                       cv_results = all_results$cv_results,
#'                       inbuilt_feature_importance = inbuilt_feature_importance,
#'                       test_metrics = if (perform_test_split) test_metrics_df else list(),
#'                       cv_mean_score = cv_score_summary_df,
#'                       permutation_importance = if (calculate_permutation_importance) mean_permutation_importances else list()))
#' }
