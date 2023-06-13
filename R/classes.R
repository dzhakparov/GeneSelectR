#' PipelineResults class
#'
#' A class to hold the results of fitting and evaluating pipelines.
#'
#' @slot fitted_pipelines A list of the fitted pipelines.
#' @slot cv_results A list of the cross-validation results for each pipeline.
#' @slot mean_feature_importances A list of the mean feature importances for each method across all splits.
#' @slot test_metrics A data.frame containing metrics (F1, accuracy, precision and recall) calculated on the unseen test set. Contains mean values across splits as well as standard deviation.
#' @exportClass PipelineResults
setClass("PipelineResults",
         slots = list(
           fitted_pipelines = "list",
           cv_results = "list",
           mean_feature_importances = "list",
           test_metrics = 'data.frame'
         ))
