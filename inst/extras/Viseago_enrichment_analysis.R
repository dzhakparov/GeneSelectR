#' Perform gene set enrichment analysis using ViSEAGO and topGO
#' Old ViSEAGO asset
#' @description This function performs gene set enrichment analysis using ViSEAGO and topGO.
#' The functionality is largely based on methods from the ViSEAGO package by the following authors:
#' Amélie L. F. Gaudin, Marie Chabaud, Bruno Spataro, Christophe Klopp, Thibaut Jombart, Stéphanie Sidibé-Bocs.
#'
#' @param gene_sets A list of named character vectors containing gene sets.
#' @param background A character vector representing the background gene set.
#' @param organism A character string corresponding to the organism of interest. Default: "org.Hs.eg.db" (for human).
#' @param ont A character string representing GO term ontology. Default: "BP" (for Biological Process).
#' @param nodeSize An integer representing the minimum number of annotated genes for a GO term. Default: 5.
#' @param algorithm A character string specifying the algorithm to use in topGO. Default: "elim".
#' @param statistic A character string specifying the statistical test to use in topGO. Default: "fisher".
#' @param cutOff A numeric value specifying the significance cutoff in topGO. Default: 0.01.
#' @param mergeCutOff A numeric value specifying the significance cutoff for merging results in ViSEAGO. Default: 0.05.
#' @return A data frame containing the merged enrichment results for all gene sets.
#' @importFrom ViSEAGO Bioconductor2GO annotate create_topGOdata merge_enrich_terms
#' @importFrom topGO runTest
#' @importFrom dplyr setdiff
#' @references To use ViSEAGO in published research, please cite:
#'             [citation]
#'@keywords hidden
#' \dontrun{
#' # Assuming all.genes and gene_sets are defined
#' background = as.character(all.genes$entrezid)
#' BP_sResults <- perform_GO_enrichment_analysis(gene_sets, background, organism = "org.Hs.eg.db")
#'}
viseaGO_enrichment_analysis <- function(gene_sets, background, organism = "org.Hs.eg.db", ont = "BP", nodeSize = 5, algorithm = "elim", statistic = "fisher", cutOff = 0.01, mergeCutOff = 0.05) {
  # Check if gene_sets is a list of named vectors. If not, raise an error.
  if (!is.list(gene_sets) || any(!sapply(gene_sets, is.vector)) || any(is.na(names(gene_sets)))) {
    stop("gene_sets must be a list of named character vectors.")
  }

  # Connect to Bioconductor and retrieve the GO annotations.
  Bioconductor <- ViSEAGO::Bioconductor2GO()

  # Load GO annotations from Bioconductor using the provided organism.
  myGENE2GO <- ViSEAGO::annotate(
    id = organism,
    object = Bioconductor
  )

  input_list <- list()

  # Iterate through the names of the gene_sets list, excluding the "background" vector.
  gene_set_names <- dplyr::setdiff(names(gene_sets), "background")
  for (gene_set_name in gene_set_names) {
    # Print the name of the current gene set for debugging purposes.
    message(paste0('Performing GO Enrichment analysis for the:', gene_set_name))

    # Extract the gene set using its name.
    gene_set <- gene_sets[[gene_set_name]]

    # Create BP topGOdata for the current gene set using the ViSEAGO package.
    BP_data <- ViSEAGO::create_topGOdata(
      geneSel = gene_set,        # Selected gene set
      allGenes = background,     # Background gene set
      gene2GO = myGENE2GO,       # GO annotations
      ont = ont,                 # GO term ontology
      nodeSize = nodeSize        # Minimum number of annotated genes for a GO term
    )

    # Run the enrichment test using the topGO package.
    elim_test <- topGO::runTest(
      BP_data,                   # Input topGOdata object
      algorithm = algorithm,     # Algorithm to use
      statistic = statistic,     # Statistical test to use
      cutOff = cutOff            # Significance cutoff
    )

    # Assign the BP_data and elim_test results to the function's environment
    assign(paste0("BP_", gene_set_name), BP_data, envir = environment())
    assign(paste0("elim_BP_", gene_set_name), elim_test, envir = environment())
  }

  for (method in gene_set_names) {
    input_list[[method]] <- c(
      paste0("BP_", method),
      paste0("elim_BP_", method)
    )
  }

  # Merge the topGO results using the ViSEAGO package.
  BP_sResults <- ViSEAGO::merge_enrich_terms(
    cutoff = mergeCutOff,       # Significance cutoff for merging results
    Input = input_list,
    envir = environment())

  # Return the results of the GO enrichment analysis.
  return(BP_sResults)
}

