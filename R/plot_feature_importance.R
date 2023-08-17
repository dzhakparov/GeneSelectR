#' Plot Feature Importance
#'
#' This function plots the feature importance scores from `mean_feature_importances` and `permutation_importances` in the `PipelineResults` object.
#'
#' @param pipelineresults An object of class `PipelineResults`.
#' @param top_n_features An integer specifying the top N features to plot based on their mean importance.
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip theme_minimal labs
#' @importFrom dplyr arrange slice desc
#' @importFrom cowplot plot_grid
#' @return A list of grid of ggplot objects.
#' @export

plot_feature_importance <- function(pipelineresults, top_n_features = 10) {

  plots_list <- list()

  for(method_name in names(pipelineresults@mean_feature_importances)){
    # Extract mean_feature_importances and permutation_importances from the PipelineResults object
    mean_importances <- pipelineresults@mean_feature_importances[[method_name]]
    permutation_importances <- pipelineresults@permutation_importances[[method_name]]

    # Order by mean_importance and take top_n_features
    top_mean_features <- mean_importances %>%
      dplyr::arrange(dplyr::desc(mean_importance)) %>%
      dplyr::slice(1:top_n_features)
    top_permutation_features <- permutation_importances %>%
      dplyr::arrange(dplyr::desc(mean_importance)) %>%
      dplyr::slice(1:top_n_features)

    # Find the intersecting features
    intersecting_features <- intersect(top_mean_features$feature, top_permutation_features$feature)

    # Create a vector of colors only for intersecting features
    color_vector <- if(length(intersecting_features) >= 3) {
      setNames(RColorBrewer::brewer.pal(length(intersecting_features), "Set1"), intersecting_features)
    } else {
      setNames(grDevices::rainbow(length(intersecting_features)), intersecting_features)
    }

    # Create a new color column for both data frames. If the feature is not in intersecting_features, color it gray.
    top_mean_features$color <- ifelse(top_mean_features$feature %in% intersecting_features, color_vector[as.character(top_mean_features$feature)], "purple")
    top_permutation_features$color <- ifelse(top_permutation_features$feature %in% intersecting_features, color_vector[as.character(top_permutation_features$feature)], "lightgreen")

    # Create the plots
    p1 <- ggplot2::ggplot(top_mean_features, ggplot2::aes(x = reorder(.data$feature, .data$mean_importance), y = .data$mean_importance)) +
      ggplot2::geom_bar(stat = "identity", aes(fill = color)) +
      ggplot2::scale_fill_identity() +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold")) +
      ggplot2::labs(x = "Features", y = "Importance", title = paste("Inbuilt Feature Importance for", method_name))

    p2 <- ggplot2::ggplot(top_permutation_features, ggplot2::aes(x = reorder(.data$feature, .data$mean_importance), y = .data$mean_importance)) +
      ggplot2::geom_bar(stat = "identity", aes(fill = color)) +
      ggplot2::scale_fill_identity() +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold")) +
      ggplot2::labs(x = "Features", y = "Importance", title = paste("Permutation Feature Importance for", method_name))

    # Arrange the plots in a grid using plot_grid from the cowplot package
    g <- cowplot::plot_grid(p1, p2, ncol = 2, labels = c("Inbuilt", "Permutation"))

    plots_list[[method_name]] <- g
  }

  return(plots_list)
}
