#' Get Child Nodes From Cluster Summaries
#'
#' This function retrieves child nodes for given parent nodes (GO IDs) and filters them according to given cluster summaries.
#'
#' @param go_ids A character vector of GO IDs to retrieve child nodes for.
#' @param cluster_summaries A named list of cluster summary data frames.
#'
#' @return A named list of data frames, where each data frame contains the child nodes of a given GO ID that appear in a cluster's summary.
#' @importFrom GOfuncR get_child_nodes
#'
#' @examples
#' \dontrun{
#' go_ids <- c('GO:0002376', 'GO:0044419')
#' cluster_summaries <- list(
#'   lasso = lasso.cluster_summary,
#'   rfe_lr = rfe_lr.cluster_summary,
#'   rfe_rf = rfe_rf.cluster_summary,
#'   uni = uni.cluster_summary,
#'   dge = dge.cluster_summary
#' )
#' results <- getChildNodesFromClusterSummaries(go_ids, cluster_summaries)
#' }
#' @export
getChildNodes <- function(go_ids, cluster_summaries) {

  parent.nodes = lapply(go_ids, GOfuncR::get_child_nodes)
  names(parent.nodes) <- go_ids

  results <- list()

  for(go_id in go_ids){
    for(cluster_name in names(cluster_summaries)){
      result_name <- paste(cluster_name, go_id, sep=".")
      results[[result_name]] <- parent.nodes[[go_id]][parent.nodes[[go_id]]$child_go_id %in% cluster_summaries[[cluster_name]]$ID,]
    }
  }

  return(results)
}
