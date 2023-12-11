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
#' @return The result of the `simplifyGOFromMultipleLists` function, typically comprising a heatmap or other
#'         visualization that displays the simplified GO enrichment results. The specific output format depends
#'         on the chosen semantic similarity measure and clustering method.
#'
#' @references For more information on the simplifyEnrichment package, see the original publication:
#'             Gu Z, Hübschmann D. simplifyEnrichment: A Bioconductor Package for Clustering and Visualizing Functional Enrichment Results.
#'             Genomics Proteomics Bioinformatics. 2023 Feb;21(1):190-202. doi: 10.1016/j.gpb.2022.04.008.
#'             Epub 2022 Jun 6. PMID: 35680096; PMCID: PMC10373083.
#'
#' @examples
#' \donttest{
#' # Mock GO enrichment results for two feature selection methods
#' fs_GO_results <- list(
#'   method1 = list(result = data.frame(GO_ID = c("GO:0008150", "GO:0009987"),
#'                                     Description = c("Biological Process 1", "Biological Process 2"),
#'                                     'p.adjust' = c(0.01, 0.02))),
#'   method2 = list(result = data.frame(GO_ID = c("GO:0008150", "GO:0008152"),
#'                                     Description = c("Biological Process 1", "Biological Process 3"),
#'                                     'p.adjust' = c(0.03, 0.04)))
#' )
#'
#' # Run the wrapper function with mock data
#' enrichment_result <- run_simplify_enrichment(fs_GO_results,
#'                                              padj_column = 'p.adjust',
#'                                              padj_cutoff = 0.05,
#'                                              ont = "BP",
#'                                              measure = "Wang",
#'                                              method = "kmeans")
#' print(enrichment_result)
#' }
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
