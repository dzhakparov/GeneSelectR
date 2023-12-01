#' @title Perform gene set enrichment analysis using clusterProfiler
#'
#' @description This function performs gene set enrichment analysis on a list of gene sets extracted from an AnnotatedGeneLists object.
#'
#' @param annotated_gene_lists An AnnotatedGeneLists object containing a list of GeneList objects.
#' @param list_type A type of AnnotatedGeneList from annotate_gene_lists function. Either 'inbuilt' or 'permutation'. (default: 'inbuilt')
#' @param background A character vector representing the background gene set.
#' @param organism A character string corresponding to the organism of interest. Default: "org.Hs.eg.db" (for human).
#' @param keyType A character string indicating the type of gene identifiers. Default: "ENTREZID".
#' @param ont A character string representing GO term ontology. Default: "BP" (for Biological Process).
#' @param pvalueCutoff A numeric value specifying the significance cutoff. Default: 0.05.
#' @param qvalueCutoff A numeric value specifying the q-value cutoff. Default: 0.2.
#' @param pAdjMethod A p-value adjustment method. Should be the one from "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". Default is 'fdr
#' @param ... Other parameters to be passed to clusterProfiler::enrichGO function.
#' @return A list containing the enrichment results for each gene set.
#'         Each element in the list is named after a gene set and contains an object produced by the `enrichGO` function from `clusterProfiler`.
#'         This object includes various details about the enrichment analysis, such as enriched GO terms, their associated p-values, q-values, and other relevant statistics.
#'         The list excludes results for the "background" gene set, focusing only on the gene sets of interest.
#'
#' @importFrom dplyr setdiff
#' @importFrom methods as slot
#'
#' @references To use clusterProfiler in published research, please cite:
#'             Yu G, Wang LG, Han Y, He QY. clusterProfiler: an R package for comparing biological themes among gene clusters. OMICS: A Journal of Integrative Biology. 2012;16(5):284-287. doi:10.1089/omi.2011.0118.
#'
#' @examples
#' \donttest{
#' # Creating a mock AnnotatedGeneLists object
#' gene_symbols <- c("GENE1", "GENE2", "GENE3")
#' ensembl_ids <- c("ENSG000001", "ENSG000002", "ENSG000003")
#' entrez_ids <- c("1001", "1002", "1003")
#'
#' create_mock_gene_list <- function() {
#'   new("GeneList",
#'        SYMBOL = gene_symbols,
#'        ENSEMBL = ensembl_ids,
#'        ENTREZID = entrez_ids)
#' }
#'
#' mock_gene_list1 <- create_mock_gene_list()
#' mock_gene_list2 <- create_mock_gene_list()
#'
#' annotated_gene_lists <- new("AnnotatedGeneLists",
#'                             inbuilt = list(Lasso = mock_gene_list1,
#'                                            Univariate = mock_gene_list2),
#'                             permutation = list(Lasso = mock_gene_list1,
#'                                                Univariate = mock_gene_list2))
#'
#' # Define a background gene set
#' background <- c("GENE1", "GENE2", "GENE3")
#'
#' # Perform GO enrichment analysis
#' results <- GO_enrichment_analysis(annotated_gene_lists,
#'                                   background = background,
#'                                   organism = "org.Hs.eg.db",
#'                                   keyType = "SYMBOL")
#' print(results)
#' }
#' @export

#' @export
GO_enrichment_analysis <- function(annotated_gene_lists, list_type = 'inbuilt', background = NULL, organism = "org.Hs.eg.db", keyType = "ENTREZID", ont = "BP", pvalueCutoff = 0.05, qvalueCutoff = 0.2, pAdjMethod  = 'fdr', ...) {

  if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
    stop("The clusterProfiler package is required but not installed. Please install it first.")
  }

  if (list_type == 'inbuilt'){
   annotated_list <-  annotated_gene_lists@inbuilt
  } else if (list_type == 'permutation'){
    annotated_list <-  annotated_gene_lists@permutation
  } else (stop("list_type should be 'inbuilt' or 'permutation'"))

  gene_sets <- lapply(annotated_list, function(gl) {

    # Check if gl is an object of class GeneList
    if (!inherits(gl, "GeneList")) {
      stop("Each element in annotated_gene_lists@annotated_lists must be an object of class GeneList.")
    }

    gl <- methods::as(gl, "GeneList")
    methods::slot(gl, keyType) # fetch the gene list based on the slot
  })

  results_list <- list()

  # Exclude the "background" gene set
  gene_set_names <- dplyr::setdiff(names(gene_sets), "background")

  for (gene_set_name in gene_set_names) {
    message(paste0('Performing GO Enrichment analysis for the:', gene_set_name))

    gene_set <- gene_sets[[gene_set_name]]

    enrichment_result <- clusterProfiler::enrichGO(
      gene         = gene_set,
      universe     = background,
      OrgDb        = organism,
      keyType      = keyType,
      ont          = ont,
      pAdjustMethod = pAdjMethod,
      pvalueCutoff = pvalueCutoff,
      qvalueCutoff = qvalueCutoff,
      ...
    )

    results_list[[gene_set_name]] <- enrichment_result
  }

  return(results_list)
}
