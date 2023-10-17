#' @title Calculate Overlap and Similarity Coefficients between Feature Lists
#' @description This function calculates the Overlap, Jaccard, and Soerensen-Dice coefficients to quantify
#' the similarity between feature lists. In addition to feature importance and permutation importance,
#' you can provide a custom list of feature names to be included in the overlap calculation.
#' @param pipeline_results A PipelineResults object containing the fitted pipelines, cross-validation results, selected features,
#'   mean performance, and mean feature importances.
#' @param custom_lists An optional named list of character vectors. Each character vector should contain feature names.
#'   The names of the list will be used as names in the resulting overlap coefficient matrices.
#' @return A list of lists of matrices showing the Overlap, Jaccard, and Soerensen-Dice coefficients for the feature lists.
#'   This includes coefficients for feature importance, permutation importance (if present), and custom lists (if provided).
#' @importFrom tmod modOverlaps
#' @export
calculate_overlap_coefficients <- function(pipeline_results, custom_lists = NULL) {
  # Check if input object belongs to the PipelineResults class
  if (!inherits(pipeline_results, "PipelineResults")) {
    stop("The input object does not belong to the PipelineResults class.")
  }

  # Function to create feature lists and calculate coefficients
  get_coefficients <- function(importances) {
    # Create feature lists
    feature_lists <- lapply(importances, function(x) x)
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

  # Extract feature names from mean_feature_importances
  feature_importances_list <- lapply(pipeline_results@inbuilt_feature_importance, function(df) df$feature)

  # Check if permutation_importances exist and apply function
  permutation_importance_coefficients <- NULL
  if (length(pipeline_results@permutation_importance) != 0) {
    # Extract feature names from permutation_importances
    permutation_importances_list <- lapply(pipeline_results@permutation_importance, function(df) df$feature)
  } else {
    permutation_importances_list <- NULL
  }


  if (!is.null(permutation_importances_list)) {
    all_lists <- list("features" = feature_importances_list, "permutations" = permutation_importances_list)
    } else all_lists <- list("features" = feature_importances_list)

  if (!is.null(custom_lists)) {
    all_lists <- lapply(all_lists, function(lst) c(lst, custom_lists))
  }

  result_lists <- lapply(all_lists, get_coefficients)

  if (!is.null(permutation_importances_list)) {
    names(result_lists) <- c("inbuilt_feature_importance_coefficient", "permutation_importance_coefficients")
    } else names(result_lists) <- c('inbuilt_feature_importance_coefficient')

  # Return both lists of coefficients
  return(result_lists)
}
