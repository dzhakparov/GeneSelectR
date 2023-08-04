#' Get Child Nodes From Cluster Summaries
#'
#' This function retrieves child nodes for given parent nodes (GO IDs) and filters them according to given cluster summaries. It also calculates the fraction of the child terms retrieved from the specified go ID in the cluster summaries.
#'
#' @param go_ids A character vector of GO IDs to retrieve child nodes for.
#' @param cluster_summaries A named list of cluster summary data frames.
#'
#' @return A named list of data frames, where each data frame contains the child nodes of a given GO ID that appear in a cluster's summary, and a list of the fraction of the child terms for each GO ID that were found in the cluster summaries.
#' @importFrom GO.db GOTERM
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
#' results <- get_child_nodes(go_ids, cluster_summaries)
#' }
#' @export

get_child_nodes <- function(go_ids, cluster_summaries) {
  # Get child terms from GO.db
  getChildTerms <- function(go_id) {
    go_offspring <- as.list(GO.db::GOTERM)
    go_offspring <- go_offspring[[go_id]]
    go_offspring <- unlist(lapply(go_offspring@goOffspring, function(x) as.character(x)))
    return(go_offspring)
  }

  # Get the child nodes for each go_id
  child_nodes <- lapply(go_ids, getChildTerms)

  # Initialize an empty list to store results
  results <- list()
  fraction_results <- list()

  # For each go_id and each cluster summary, get the child nodes that appear in the cluster summary
  for(go_id in go_ids) {
    for(cluster_name in names(cluster_summaries)) {
      result_name <- paste(cluster_name, go_id, sep=".")
      matched_child_nodes <- child_nodes[[go_id]][child_nodes[[go_id]] %in% cluster_summaries[[cluster_name]]$ID]
      results[[result_name]] <- matched_child_nodes

      # Calculate the fraction of child nodes that appear in the cluster summary
      fraction_results[[result_name]] <- length(matched_child_nodes) / length(child_nodes[[go_id]])
    }
  }

  return(list('results' = results, 'fractions' = fraction_results))
}

