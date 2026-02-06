#' Plot Feature Importance
#'
#' This function plots the feature importance scores from `inbuilt_feature_importance` and `permutation_importance` in the `PipelineResults` object.
#'
#' @param pipelineresults An object of class `PipelineResults`.
#' @param top_n_features An integer specifying the top N features to plot based on their mean importance.
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip theme_minimal labs
#' @importFrom dplyr arrange slice desc
#' @importFrom cowplot plot_grid
#' @importFrom stats reorder
#' @return A list of grid of ggplot objects.
#' @export

plot_feature_importance <- function(pipelineresults, top_n_features = 10) {

  plots_list <- list()

  for(method_name in names(pipelineresults@inbuilt_feature_importance)){

    # Extract inbuilt_feature_importance from the PipelineResults object
    mean_importance <- pipelineresults@inbuilt_feature_importance[[method_name]]

    # Order by mean_importance and take top_n_features
    top_mean_features <- mean_importance %>%
      dplyr::arrange(dplyr::desc(mean_importance)) %>%
      dplyr::slice(1:top_n_features)

    # Create the plot for inbuilt feature importance
    p1 <- ggplot2::ggplot(top_mean_features, ggplot2::aes(x = stats::reorder(.data$feature, .data$mean_importance), y = .data$mean_importance)) +
      ggplot2::geom_bar(stat = "identity", fill = "purple") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold")) +
      ggplot2::labs(x = "Features", y = "Importance", title = paste("Inbuilt Feature Importance for", method_name))

    # Check if permutation_importance is provided
    if (!is.null(pipelineresults@permutation_importance[[method_name]])) {
      permutation_importance <- pipelineresults@permutation_importance[[method_name]]
      top_permutation_features <- permutation_importance %>%
        dplyr::arrange(dplyr::desc(mean_importance)) %>%
        dplyr::slice(1:top_n_features)

      # Create the plot for permutation feature importance
      p2 <- ggplot2::ggplot(top_permutation_features, ggplot2::aes(x = reorder(.data$feature, .data$mean_importance), y = .data$mean_importance)) +
        ggplot2::geom_bar(stat = "identity", fill = "lightgreen") +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(x = "Features", y = "Importance", title = paste("Permutation Feature Importance for", method_name))

      # Arrange the plots in a grid using plot_grid from the cowplot package
      g <- cowplot::plot_grid(p1, p2, ncol = 2, labels = c("Inbuilt", "Permutation"))
    } else {
      g <- p1
    }

    plots_list[[method_name]] <- g
  }

  return(plots_list)
}

