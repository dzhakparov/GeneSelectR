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
           permutation_importances = 'list',
           #gene_set_stability = 'list',
           cv_mean_score = 'data.frame',
           test_metrics = 'data.frame'
         ))

#' @title GeneList class
#' @description A class to hold annotated gene list for a single method.
#' @slot gene_name A character vector of gene names.
#' @slot ensembl_id A character vector of Ensembl IDs.
#' @slot entrezid A character vector of Entrez IDs.
#' @exportClass GeneList
setClass("GeneList",
         slots = list(
           SYMBOL = "character",
           ENSEMBL = "character",
           ENTREZID = "character"
         ))


#'
#' @title AnootatedGeneLists class
#' @description A class to hold a list of GeneList objects, each representing a method.
#' @slot gene_lists A list of GeneList objects.
#' @exportClass AnnotatedGeneLists
setClass("AnnotatedGeneLists",
         slots = list(
           annotated_lists = "list"
         ))
