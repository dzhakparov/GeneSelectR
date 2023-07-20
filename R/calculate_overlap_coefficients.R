#' @title Calculate Overlap and Similarity Coefficients between Feature Lists
#' @description This function calculates the Overlap, Jaccard, and Soerensen-Dice coefficients to quantify
#' the similarity between feature lists.
#' @param pipeline_results A PipelineResults object containing the fitted pipelines, cross-validation results, selected features,
#'   mean performance, and mean feature importances.
#' @return A list of matrices showing the Overlap, Jaccard, and Soerensen-Dice coefficients for the feature lists for feature importance and permutation importance scores if they are provided
#' @importFrom tmod modOverlaps
#' @export
# calculate_overlap_coefficients <- function(pipeline_results) {
#   # Check if input object belongs to the PipelineResults class
#   if (!inherits(pipeline_results, "PipelineResults")) {
#     stop("The input object does not belong to the PipelineResults class.")
#   }
#
#   # Create feature lists
#   feature_lists <- lapply(pipeline_results@mean_feature_importances, function(df) df$feature)
#   names(feature_lists) <- names(pipeline_results@mean_feature_importances)
#
#   # Calculate the overlap coefficients and round to 2 decimal places
#   calculate_coefficients <- function(stat) {
#     coefficients <- tmod::modOverlaps(modules = feature_lists, mset = NULL, stat = stat)
#     round(coefficients, 2)
#   }
#
#   overlap.coef_features <- calculate_coefficients("overlap")
#   j.coef_features <- calculate_coefficients("jaccard")
#   s.coef_features <- calculate_coefficients("soerensen")
#
#   # Return the results as a list of matrices
#   return(list(overlap = overlap.coef_features, jaccard = j.coef_features, soerensen = s.coef_features))
# }

calculate_overlap_coefficients <- function(pipeline_results) {
  # Check if input object belongs to the PipelineResults class
  if (!inherits(pipeline_results, "PipelineResults")) {
    stop("The input object does not belong to the PipelineResults class.")
  }

  # Function to create feature lists and calculate coefficients
  get_coefficients <- function(importances) {
    # Create feature lists
    feature_lists <- lapply(importances, function(df) df$feature)
    names(feature_lists) <- names(importances)

    # Calculate the overlap coefficients and round to 2 decimal places
    calculate_coefficients <- function(stat) {
      coefficients <- tmod::modOverlaps(modules = feature_lists, mset = NULL, stat = stat)
      round(coefficients, 2)
    }

    overlap.coef_features <- calculate_coefficients("overlap")
    j.coef_features <- calculate_coefficients("jaccard")
    s.coef_features <- calculate_coefficients("soerensen")

    # Return the results as a list of matrices
    return(list(overlap = overlap.coef_features, jaccard = j.coef_features, soerensen = s.coef_features))
  }

  # Apply function to mean_feature_importances
  feature_importance_coefficients <- get_coefficients(pipeline_results@mean_feature_importances)

  # Check if permutation_importances exist and apply function
  permutation_importance_coefficients <- NULL
  if (!is.null(pipeline_results@permutation_importances)) {
    permutation_importance_coefficients <- get_coefficients(pipeline_results@permutation_importances)
  }

  # Return both lists of coefficients
  return(list(feature_importance_coefficients = feature_importance_coefficients,
              permutation_importance_coefficients = if (!is.null(permutation_importance_coefficients)) permutation_importance_coefficients  else list()))
}
