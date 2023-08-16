#' Retrieve and Plot the Offspring Nodes of GO Terms
#'
#' This function retrieves the children nodes for a set of Gene Ontology (GO) terms
#' from a list of GO terms and can plot the offspring nodes' numbers and fractions for each term.
#'
#' @param GO_data A list of GO data where each element corresponds to a different feature list.
#'                Each element should have a @result data frame with a column 'ID' containing GO terms.
#' @param GO_terms A character vector containing GO term IDs for which offspring nodes are to be fetched.
#' @param ontology A character string specifying the type of ontology to be considered.
#'                 Can be one of 'BP' (Biological Process), 'MF' (Molecular Function), or 'CC' (Cellular Component).
#'                 Default is 'BP'.
#' @param plot A logical. If TRUE, the function plots the number and fraction of offspring nodes
#'             for each term in `GO_terms` across all feature lists in `GO_data`.
#'             Default is FALSE.
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item \strong{feature_list} - The names of the feature lists from `GO_data`.
#'   \item \strong{all_terms_number} - The total number of GO terms in each feature list.
#'   \item \strong{offspring_nodes_number} - The number of offspring nodes for the given GO term(s) in each feature list.
#'   \item \strong{offspring_terms} - The actual offspring terms for the given GO term(s) in each feature list, concatenated by ';'.
#'   \item \strong{fraction} - The fraction (percentage) of offspring nodes out of all terms in each feature list.
#'   \item \strong{GO_term} - The GO term being considered.
#' }
#'
#' @examples
#' \dontrun{
#' GO_terms_vec <- c("GO:0002376", "GO:0008150")
#' df_res <- get_children_nodes(GO_data = all_selection.GO_inbuilt, GO_terms = GO_terms_vec, plot = TRUE)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal labs theme
#' @importFrom GO.db GOBPOFFSPRING GOMFOFFSPRING GOCCOFFSPRING
#'
#' @export

compute_GO_child_term_metrics <- function(GO_data, GO_terms, ontology = 'BP', plot = FALSE) {

  # Determine the appropriate DAG based on ontology
  get_dag <- function(ontology) {
    if (ontology == 'BP') {
      return(as.list(GO.db::GOBPOFFSPRING))
    } else if (ontology == 'MF') {
      return(as.list(GO.db::GOMFOFFSPRING))
    } else if (ontology == 'CC') {
      return(as.list(GO.db::GOCCOFFSPRING))
    } else {
      message('Ontology should be one of "BP", "MF" or "CC"')
      return(NULL)
    }
  }
  dag <- get_dag(ontology)

  df_result <- data.frame()

  for (term in GO_terms) {
    dag_term <- dag[[term]]
    res <- data.frame(feature_list = names(GO_data),
                      all_terms_number = sapply(GO_data, function(x) length(x@result$ID)),
                      offspring_nodes_number = sapply(GO_data, function(x) length(dag_term[dag_term %in% x@result$ID])),
                      offspring_terms = sapply(GO_data, function(x) paste(dag_term[dag_term %in% x@result$ID], collapse = ";")),
                      stringsAsFactors = FALSE)
    res$fraction <- (res$offspring_nodes_number / res$all_terms_number) * 100
    res$GO_term <- term
    df_result <- rbind(df_result, res)
  }

  if (plot) {
    # Plotting Offspring Nodes Number
    p1 <- ggplot2::ggplot(df_result, ggplot2::aes(x = feature_list, y = offspring_nodes_number, fill = GO_term)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Offspring Nodes Number per Feature List",
                    y = "Number of Offspring Nodes",
                    x = "Feature List") +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p1)

    # Plotting Fraction
    p2 <- ggplot2::ggplot(df_result, ggplot2::aes(x = feature_list, y = fraction, fill = GO_term)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Fraction of Offspring Nodes per Feature List",
                    y = "Fraction (%)",
                    x = "Feature List") +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p2)
  }

  return(df_result)
}

