#' Generate Heatmaps for Semantic Similarity Analysis
#' old ViSEAGO asset
#' This function creates and returns heatmaps of Gene Ontology (GO) terms and clusters using the ViSEAGO package.
#'
#' @param GO_SS_and_clusters A list containing "GO_SS" and "GO_cluster_SS", which are objects containing semantic similarity distances.
#' @param tree_distance The distance metric for hierarchical clustering of GO terms and clusters (default: "Wang").
#' @param tree_aggreg_method The agglomeration method for hierarchical clustering of GO terms and clusters (default: "ward.D2").
#' @param heatmap_dynamic_deepSplit The deepSplit argument for dynamicTreeCut::cutreeDynamic (default: 2).
#' @param heatmap_dynamic_minClusterSize The minimum cluster size for dynamicTreeCut::cutreeDynamic (default: 10).
#' @return A list containing two elements: "GO_terms_heatmap" and "GO_clusters_heatmap", which are objects of class 'GOTermsHeatmap' and 'GOClustersHeatmap' respectively.
#' @references To use ViSEAGO in published research, please cite:
#'             [citation]
#'
#' @importFrom ViSEAGO GOterms_heatmap GOclusters_heatmap
#' @export
#' @examples
#' \dontrun{
#' results_heatmap <- generate_GO_heatmaps(GO_SS_and_clusters)
#' }
#' @keywords hidden internal
generate_GO_heatmaps <- function(GO_SS_and_clusters,
                                 tree_distance = "Wang",
                                 tree_aggreg_method = "ward.D2",
                                 heatmap_dynamic_deepSplit = 2,
                                 heatmap_dynamic_minClusterSize = 10,
                                 distance_GO_cluster_SS = 'BMA',
                                 GO_terms_distance = 'BMA') {

  GO_SS <-GO_SS_and_clusters$GO_SS

  GO_terms_heatmap <- ViSEAGO::GOterms_heatmap(
    myGOs = GO_SS,
    showIC = TRUE,
    showGOlabels = F,
    GO.tree = list(
      tree = list(
      distance = tree_distance,
      aggreg.method = tree_aggreg_method),
    cut = list(
      dynamic = list(
        pamStage = TRUE,
        pamRespectsDendro = TRUE,
        deepSplit = heatmap_dynamic_deepSplit,
        minClusterSize = heatmap_dynamic_minClusterSize))

    )
  )


  #GO_terms_heatmap <- ViSEAGO::GOterms_heatmap(GO_SS)

  # Semantic similarity between clusters
  GO_terms_heatmap <- ViSEAGO::compute_SS_distances(GO_terms_heatmap, distance = distance_GO_cluster_SS)

  GO_clusters_heatmap <- ViSEAGO::GOclusters_heatmap(
    GO_terms_heatmap,
    tree = list(distance = distance_GO_cluster_SS,
                aggreg.method = tree_aggreg_method))

  # # Add class attribute
  # class(GO_terms_heatmap) <- "GOTermsHeatmap"
  # class(GO_clusters_heatmap) <- "GOClustersHeatmap"

  return(list(GO_terms_heatmap = GO_terms_heatmap, GO_clusters_heatmap = GO_clusters_heatmap))
}
