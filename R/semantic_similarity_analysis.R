#' Calculate Semantic Similarity Distances
#'
#' This function calculates semantic similarity distances between Gene Ontology (GO) terms using the ViSEAGO package.
#'
#' @param enriched_results A data frame of enriched GO terms.
#' @param distance_GO_SS The distance metric for semantic similarity analysis of GO terms (default: "Wang").
#' @param distance_GO_cluster_SS The distance metric for semantic similarity analysis of GO clusters (default: "BMA").
#' @param organism A character string corresponding to the organism of interest. Default: "org.Hs.eg.db" (for human).
#' @return A list containing two elements: "GO_SS" and "GO_cluster_SS", which are objects containing semantic similarity distances.
#' @references To use ViSEAGO in published research, please cite:
#'             [citation]
#'
#' @importFrom ViSEAGO Bioconductor2GO annotate build_GO_SS compute_SS_distances
#' @export
#' @examples
#' \dontrun{
#' GO_SS_and_clusters <- compute_GO_SS_distances(enriched_results)
#' }
semantic_similarity_analysis <- function(enriched_results, distance_GO_SS = "Wang", distance_GO_cluster_SS = "Wang", organism = "org.Hs.eg.db") {

  # Retrieve GO annotations
  Bioconductor <- ViSEAGO::Bioconductor2GO()
  myGENE2GO <- ViSEAGO::annotate(id = organism, object = Bioconductor)

  # Semantic similarity analysis
  GO_SS <- ViSEAGO::build_GO_SS(gene2GO = myGENE2GO, enrich_GO_terms = enriched_results)
  GO_SS <- ViSEAGO::compute_SS_distances(GO_SS, distance = distance_GO_SS)
  print(GO_SS)
  # Semantic similarity between clusters
  GO_cluster_SS <- ViSEAGO::compute_SS_distances(GO_SS, distance = distance_GO_cluster_SS)
  print(GO_cluster_SS)

  return(list(GO_SS = GO_SS, GO_cluster_SS = GO_cluster_SS))
}
