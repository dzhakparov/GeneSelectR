#' Perform gene set enrichment analysis using ViSEAGO and topGO
#'
#' This function takes a PipelineResults object containing selected_features
#' as a named list of data frames with a "features" column, a background gene set,
#' GO annotations, and optionally additional gene sets provided by the user.
#' It performs gene set enrichment analysis for each gene set and returns the merged results.
#'
#' @param pipeline_results A PipelineResults object containing a named list of data frames
#'                         with a "features" column as gene sets.
#' @param background A character vector representing the background gene set.
#' @param myGENE2GO A data frame containing GO annotations from Bioconductor.
#' @param additional_gene_sets (optional) A named list of character vectors containing
#'                              additional user-defined gene sets.
#'
#' @return A data frame containing the merged enrichment results for all gene sets.
#'
#' @importFrom ViSEAGO Bioconductor2GO annotate create_topGOdata merge_enrich_terms
#' @importFrom topGO runTest
#' @importFrom dplyr setdiff
#' @examples
#' # Assuming all.genes, PipelineResults, and myGENE2GO are defined
#' background = as.character(all.genes$entrezid)
#' BP_sResults <- perform_analysis(PipelineResults, background, myGENE2GO)
#'
#'
# Assuming 'pipeline_results' is an instance of the PipelineResults class
# The perform_GO_enrichment_analysis function takes a list of gene_sets and a background gene set
# as input and returns the results of a Gene Ontology (GO) enrichment analysis.
perform_GO_enrichment_analysis <- function(gene_sets, background) {
  # Check if gene_sets is a list of named vectors. If not, raise an error.
  if (!is.list(gene_sets) || any(!sapply(gene_sets, is.vector)) || any(is.na(names(gene_sets)))) {
    stop("gene_sets must be a list of named vectors.")
  }

  # Connect to Bioconductor and retrieve the GO annotations.
  Bioconductor <- ViSEAGO::Bioconductor2GO()

  # Load GO annotations from Bioconductor using the org.Hs.eg.db package.
  myGENE2GO <- ViSEAGO::annotate(
    "org.Hs.eg.db",
    Bioconductor
  )

  input_list <- list()

  # Iterate through the names of the gene_sets list, excluding the "background" vector.
  gene_set_names <- dplyr::setdiff(names(gene_sets), "background")
  for (gene_set_name in gene_set_names) {
    # Print the name of the current gene set for debugging purposes.
    print(gene_set_name)

    # Extract the gene set using its name.
    gene_set <- gene_sets[[gene_set_name]]

    # Create BP topGOdata for the current gene set using the ViSEAGO package.
    BP_data <- ViSEAGO::create_topGOdata(
      geneSel = gene_set,        # Selected gene set
      allGenes = background,     # Background gene set
      gene2GO = myGENE2GO,       # GO annotations
      ont = "BP",                # GO term ontology (Biological Process)
      nodeSize = 5               # Minimum number of annotated genes for a GO term
    )

    # Run the enrichment test using the topGO package with the "elim" algorithm and Fisher's test.
    elim_test <- topGO::runTest(
      BP_data,                   # Input topGOdata object
      algorithm = "elim",        # Algorithm to use (Elim)
      statistic = "fisher",      # Statistical test to use (Fisher's test)
      cutOff = 0.01              # Significance cutoff
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
    cutoff = 0.05,              # Significance cutoff for merging results
    Input = input_list,
    envir = environment())

  # Return the results of the GO enrichment analysis.
  return(BP_sResults)
}
