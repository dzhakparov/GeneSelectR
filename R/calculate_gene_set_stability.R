#' Calculate Gene Set Stability
#'
#' @description This function calculates the gene set stability for each method across different splits.
#' @importFrom reticulate source_python
#' @param selected_features A list of selected features. Each element of the list represents a split and should be a named list where the names are the methods and the values are data frames containing the feature importances for that method in that split.
#' @param X_train The original training data used for feature selection.
#'
#' @return A list of gene set stability values for each method.
#'
calculate_gene_set_stability <- function(selected_features, X_train) {
  reticulate::source_python(system.file("python/geneset_stability.py", package = "GeneSelectR"))
  gene_set_stability <- list()

  # Assuming the first split has the same methods as the others
  for (method in names(selected_features[[1]])) {
    # Extract gene sets for the current method across all splits
    gene_sets <- lapply(selected_features, function(split) {
      split[[method]]$feature
    })

    # Convert the keys to integers starting from 1
    names(gene_sets) <- seq_along(gene_sets)

    # Calculate the correlation matrix for the original training data
    correlation_matrix <- calculate_correlation_matrix(X_train)

    # Convert the correlation matrix to a Python object
    correlation_matrix_py <- reticulate::r_to_py(correlation_matrix)

    # Calculate the gene set stability for the current method
    stability <- zucknick_stability(gene_sets, correlation_matrix_py)

    # Add the gene set stability for the current method to the results list
    gene_set_stability[[method]] <- stability
  }

  return(gene_set_stability)
}
