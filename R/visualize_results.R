#' Visualize Results of ViSEAGO Analysis
#'
#' This function creates and returns heatmaps of Gene Ontology (GO) terms and clusters using the ViSEAGO package.
#'
#' @param results A data frame of enriched GO terms.
#' @param distance_GO_SS The distance metric for semantic similarity analysis of GO terms (default: "Wang").
#' @param distance_GO_cluster_SS The distance metric for semantic similarity analysis of GO clusters (default: "BMA").
#' @param tree_distance The distance metric for hierarchical clustering of GO terms and clusters (default: "Wang").
#' @param tree_aggreg_method The agglomeration method for hierarchical clustering of GO terms and clusters (default: "ward.D2").
#' @param heatmap_dynamic_deepSplit The deepSplit argument for dynamicTreeCut::cutreeDynamic (default: 2).
#' @param heatmap_dynamic_minClusterSize The minimum cluster size for dynamicTreeCut::cutreeDynamic (default: 10).
#'
#' @return A list containing two elements: "GO_terms_heatmap" and "GO_clusters_heatmap", which are objects of class 'GOTermsHeatmap' and 'GOClustersHeatmap' respectively.
#' @export
#'
#' @importFrom ViSEAGO Bioconductor2GO annotate build_GO_SS compute_SS_distances GOterms_heatmap GOclusters_heatmap
#' @examples
#' \dontrun{
#' library(GeneSelectR)
#' result_list <- visualize_results(results)
#' }
#' @references
#' See ViSEAGO for more information on the underlying functions used:
#' Lemoine F, Labedan B, Froidevaux C (2018). “ViSEAGO: a Bioconductor package for clustering biological functions using Gene Ontology and semantic similarity.” BioData Mining, 11, 24. doi: 10.1186/s13040-018-0181-9.
#'


# Custom wrapper functions
# custom_compute_SS_distances <- function(GO_SS, distance, ...) {
#   ViSEAGO::compute_SS_distances(GO_SS, distance = distance, ...)
# }
#
# custom_GOterms_heatmap <- function(GO_SS, showIC, showGOlabels, GO_tree, samples_tree, ...) {
#   ViSEAGO::GOterms_heatmap(GO_SS, showIC = showIC, showGOlabels = showGOlabels,
#                            GO.tree = GO_tree, samples.tree = samples_tree, ...)
# }
#
# custom_GOclusters_heatmap <- function(GO_cluster_SS, tree, ...) {
#   ViSEAGO::GOclusters_heatmap(GO_cluster_SS, tree = tree, ...)
# }
#
# visualize_results <- function(results,
#                               distance_GO_SS = "Wang",
#                               distance_GO_cluster_SS = "BMA",
#                               tree_distance = "Wang",
#                               tree_aggreg_method = "ward.D2",
#                               heatmap_dynamic_deepSplit = 2,
#                               heatmap_dynamic_minClusterSize = 10,
#                               compute_SS_distances_args = list(),
#                               GOterms_heatmap_args = list(),
#                               GOclusters_heatmap_args = list()) {
#
#
#   # Retrieve GO annotations
#   Bioconductor <- ViSEAGO::Bioconductor2GO()
#   myGENE2GO <- ViSEAGO::annotate("org.Hs.eg.db", Bioconductor)
#
#   # Semantic similarity analysis
#   GO_SS <- ViSEAGO::build_GO_SS(gene2GO = myGENE2GO, enrich_GO_terms = results)
#   print(class(GO_SS))
#   GO_SS <- do.call(custom_compute_SS_distances,
#                    c(list(GO_SS = GO_SS, distance = distance_GO_SS), compute_SS_distances_args))
#
#   # heatmap of GO terms
#   GO_terms_heatmap <- do.call(custom_GOterms_heatmap,
#                               c(list(GO_SS = GO_SS,
#                                      showIC = TRUE,
#                                      showGOlabels = F,
#                                      GO_tree = list(
#                                        tree = list(
#                                          distance = tree_distance,
#                                          aggreg.method = tree_aggreg_method
#                                        ),
#                                        cut = list(
#                                          dynamic = list(
#                                            pamStage = TRUE,
#                                            pamRespectsDendro = TRUE,
#                                            deepSplit = heatmap_dynamic_deepSplit,
#                                            minClusterSize = heatmap_dynamic_minClusterSize
#                                          )
#                                        )
#                                      ),
#                                      samples_tree = NULL),
#                                 GOterms_heatmap_args))
#
#   # Add class attribute
#   class(GO_terms_heatmap) <- "GOTermsHeatmap"
#
#   # Semantic similarity between clusters
#   GO_cluster_SS <- do.call(custom_compute_SS_distances, c(list(GO_SS = GO_terms_heatmap,
#                                                                distance = distance_GO_cluster_SS), compute_SS_distances_args))
#
#   # Heatmap of GO clusters
#   GO_clusters_heatmap <- do.call(custom_GOclusters_heatmap,
#                                  c(list(GO_cluster_SS = GO_cluster_SS,
#                                         tree = list(
#                                           distance = distance_GO_cluster_SS,
#                                           aggreg.method = tree_aggreg_method)),
#                                    GOclusters_heatmap_args))
#
#   # Add class attribute
#   class(GO_clusters_heatmap) <- "GOClustersHeatmap"
#
#   return(list(GO_terms_heatmap = GO_terms_heatmap, GO_clusters_heatmap = GO_clusters_heatmap))
# }

visualize_results <- function(results,
                              distance_GO_SS = "Wang",
                              distance_GO_cluster_SS = "BMA",
                              tree_distance = "Wang",
                              tree_aggreg_method = "ward.D2",
                              heatmap_dynamic_deepSplit = 2,
                              heatmap_dynamic_minClusterSize = 10) {

  # Retrieve GO annotations
  Bioconductor <- ViSEAGO::Bioconductor2GO()
  myGENE2GO <- ViSEAGO::annotate("org.Hs.eg.db", Bioconductor)

  # Semantic similarity analysis
  GO_SS <- ViSEAGO::build_GO_SS(gene2GO = myGENE2GO, enrich_GO_terms = results)
  GO_SS <- ViSEAGO::compute_SS_distances(GO_SS, distance = distance_GO_SS)

  # heatmap of GO terms
  GO_terms_heatmap <- ViSEAGO::GOterms_heatmap(GO_SS,
                                               showIC = TRUE,
                                               showGOlabels = F,
                                               GO_tree = list(
                                                 tree = list(
                                                   distance = tree_distance,
                                                   aggreg.method = tree_aggreg_method
                                                 ),
                                                 cut = list(
                                                   dynamic = list(
                                                     pamStage = TRUE,
                                                     pamRespectsDendro = TRUE,
                                                     deepSplit = heatmap_dynamic_deepSplit,
                                                     minClusterSize = heatmap_dynamic_minClusterSize
                                                   )
                                                 )
                                               ),
                                               samples_tree = NULL)

  # Add class attribute
  class(GO_terms_heatmap) <- "GOTermsHeatmap"

  # Semantic similarity between clusters
  GO_cluster_SS <- ViSEAGO::compute_SS_distances(GO_terms_heatmap,
                                                 distance = distance_GO_cluster_SS)

  # Heatmap of GO clusters
  GO_clusters_heatmap <- ViSEAGO::GOclusters_heatmap(GO_cluster_SS,
                                                     tree = list(
                                                       distance = distance_GO_cluster_SS,
                                                       aggreg.method = tree_aggreg_method))

  # Add class attribute
  class(GO_clusters_heatmap) <- "GOClustersHeatmap"

  return(list(GO_terms_heatmap = GO_terms_heatmap, GO_clusters_heatmap = GO_clusters_heatmap))
}

