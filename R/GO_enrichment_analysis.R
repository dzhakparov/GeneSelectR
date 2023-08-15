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
#' @return A list containing the enrichment results for all gene sets (excluding "background").
#'
#' @importFrom clusterProfiler enrichGO
#' @importFrom dplyr setdiff
#' @importFrom methods as slot
#'
#' @references To use clusterProfiler in published research, please cite:
#'             Yu G, Wang LG, Han Y, He QY. clusterProfiler: an R package for comparing biological themes among gene clusters. OMICS: A Journal of Integrative Biology. 2012;16(5):284-287. doi:10.1089/omi.2011.0118.
#'
#' @examples
#' \dontrun{
#' # Assuming annotated_gene_lists and background are defined
#' results <- GO_enrichment_analysis(annotated_gene_lists, background, organism = "org.Hs.eg.db", keyType = "ENTREZID")
#' }
#' @export
GO_enrichment_analysis <- function(annotated_gene_lists, list_type = 'inbuilt', background, organism = "org.Hs.eg.db", keyType = "ENTREZID", ont = "BP", pvalueCutoff = 0.05, qvalueCutoff = 0.2, pAdjMethod  = 'fdr') {

  # # Check if annotated_gene_lists is an object of class AnnotatedGeneLists
  # if (!inherits(annotated_gene_lists, "AnnotatedGeneLists")) {
  #   stop("annotated_gene_lists must be an object of class AnnotatedGeneLists.")
  # }
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
    methods::slot(gl, keyType)
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
      qvalueCutoff = qvalueCutoff
    )

    results_list[[gene_set_name]] <- enrichment_result
  }

  return(results_list)
}
