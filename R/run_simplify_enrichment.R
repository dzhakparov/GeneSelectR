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
#' @references reference
#' @return The result from the simplifyGOFromMultipleLists function, typically including a heatmap or other visualization
#'         of the simplified GO enrichment results, depending on the specified measure and method.
#'
#' @export
run_simplify_enrichment <- function(fs_GO_results, padj_column = 'p.adjust', padj_cutoff, ont, measure, method, ...) {

  if (!requireNamespace("simplifyEnrichment", quietly = TRUE)) {
    stop("The simplifyEnrichment package is required but not installed. Please install it first.")
  }
  GO_df_list <- lapply(fs_GO_results, function (x) x@result )
  names(GO_df_list) <- names(fs_GO_results)
  simplifyEnrichment::simplifyGOFromMultipleLists(GO_df_list,
                                                  padj_column=padj_column,
                                                  padj_cutoff=padj_cutoff,
                                                  ont=ont,
                                                  measure=measure,
                                                  method=method,
                                                  ...)
}
