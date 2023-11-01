#' Plot Feature Overlaps Using UpSet Plots
#'
#' This function produces separate UpSet plots for inbuilt feature importances and permutation importances,
#' allowing you to visualize the overlap of feature lists. Optionally, you can include custom lists.
#'
#' @param pipeline_results A PipelineResults object containing the fitted pipelines, cross-validation results, selected features,
#'   mean performance, and mean feature importances.
#' @param custom_lists An optional named list of character vectors. Each character vector should contain feature names.
#'   The names of the list will be used as names in the UpSet plots.
#'
#' @return A named list containing two UpSet plots accessible as \code{result$inbuilt_importance} for inbuilt feature importances
#'   and \code{result$permutation_importance} for permutation importances.
#'
#' @examples
#' \dontrun{
#' pipeline_results <- PipelineResults$new(...)
#' custom_lists <- list("custom1" = c("gene1", "gene2"), "custom2" = c("gene3", "gene4"))
#' result <- plot_feature_overlap_upset(pipeline_results, custom_lists)
#' print(result$plot1)
#' print(result$plot2)
#' }
#' @importFrom utils modifyList
#' @export
plot_upset <- function(pipeline_results, custom_lists = NULL) {
  # Check if UpSetR is installed
  if (!requireNamespace("UpSetR", quietly = TRUE)) {
    stop("UpSetR package is required.")
  }

  # Extract feature names from inbuilt_feature_importance
  feature_importances_list <- lapply(pipeline_results@inbuilt_feature_importance, function(df) df$feature)

  # Combine feature_importances_list with custom_lists if available
  if (!is.null(custom_lists)) {
    feature_importances_list <- modifyList(feature_importances_list, custom_lists)
  }

  # Create UpSet plot for feature_importances and custom lists
  plot1 <- UpSetR::upset(UpSetR::fromList(feature_importances_list),
                         sets.bar.color = "black",
                         main.bar.color = "#A2D729",
                         matrix.color = "#3C91E6",
                         keep.order = TRUE)

  # Check if permutation_importances exist
  if (length(pipeline_results@permutation_importance) != 0) {
    # Extract feature names from permutation_importances
    permutation_importances_list <- lapply(pipeline_results@permutation_importance, function(df) df$feature)

    # Combine permutation_importances_list with custom_lists if available
    if (!is.null(custom_lists)) {
      permutation_importances_list <- modifyList(permutation_importances_list, custom_lists)
    }

    # Create UpSet plot for permutation_importances and custom lists
    plot2 <- UpSetR::upset(UpSetR::fromList(permutation_importances_list),
                           sets.bar.color = "black",
                           main.bar.color = "#A2D729",
                           matrix.color = "#3C91E6",
                           keep.order = TRUE)
  } else {
    plot2 <- NULL
  }

  # Create named list to store both plots
  result <- list(
    inbuilt_importance = plot1,
    permutation_importance = plot2
  )

  return(result)
}
