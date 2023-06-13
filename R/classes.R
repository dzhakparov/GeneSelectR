#' PipelineResults class
#'
#' A class to hold the results of fitting and evaluating pipelines.
#'
#' @slot fitted_pipelines A list of the fitted pipelines.
#' @slot cv_results A list of the cross-validation results for each pipeline.
#' @slot selected_features A list of the selected features for each pipeline.
#' @slot mean_performance A data frame of the mean performance for each feature selection method across all splits.
#' @slot mean_feature_importances A list of the mean feature importances for each method across all splits.
#' @exportClass PipelineResults
setClass("PipelineResults",
         slots = list(
           fitted_pipelines = "list",
           cv_results = "list",
           selected_features = "list",
           mean_performance = "data.frame",
           mean_feature_importances = "list",
           test_metrics = 'data.frame'
         ))
