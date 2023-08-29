#' @title Run simplifyGOFromMultipleLists with specified measure and method
#'
#' @description
#' This function is simply a wrapper for the simplifyGOFromMultipleLists function in the simplifyEnrichment package,
#' created for the ease of data input. All credit for the underlying functionality goes to the authors
#' of the simplifyEnrichment package.
#'
#' @param fs_GO_results A list of dataframes containing GO enrichment results for feature selection methods. The GO object for the simplifyGOFromMultipleLists function.
#' @param padj_column Character. The column name for the p-value adjustment.
#' @param padj_cutoff Numeric. The cutoff for the p-value adjustment.
#' @param ont Character. The ontology for the simplifyGOFromMultipleLists function.
#' @param measure Character. The semantic similarity measure for the simplifyGOFromMultipleLists function.
#' @param method Character. The clustering method for the simplifyGOFromMultipleLists function.
#' @param ... Other parameters that can be passed to simplifyGOFromMultipleLists
#'
#' @return The result of the simplifyGOFromMultipleLists function.
#'
#' @importFrom simplifyEnrichment simplifyGOFromMultipleLists
#' @export
run_simplify_enrichment <- function(fs_GO_results, padj_column, padj_cutoff, ont, measure, method, ...) {
  simplifyEnrichment::simplifyGOFromMultipleLists(fs_GO_results,
                                                  padj_column=padj_column,
                                                  padj_cutoff=padj_cutoff,
                                                  ont=ont,
                                                  measure=measure,
                                                  method=method)
}
