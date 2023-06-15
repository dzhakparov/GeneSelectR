#' @title Generate Heatmaps to Visualize Overlap and Similarity Coefficients between Feature Lists
#' @description This function takes a list of matrices of overlap and similarity coefficients and generates heatmaps to visualize them.
#' @param coefficients A list of matrices showing the Overlap, Jaccard, and Soerensen-Dice coefficients for the feature lists.
#' @param save_plot A logical value indicating whether to save the heatmap plots to a file or not. Default is FALSE.
#' @param filename A character string specifying the filename for the saved heatmap plots (if save_plot = TRUE).
#' @return A grid of heatmaps showing the Overlap, Jaccard, and Soerensen-Dice coefficients for the feature lists.
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradientn theme_minimal theme labs coord_fixed
#' @importFrom reshape2 melt
#' @importFrom RColorBrewer brewer.pal
#' @importFrom cowplot plot_grid
#' @importFrom rlang .data
#' @export
generate_overlap_heatmaps <- function(coefficients, save_plot = FALSE, filename = NULL) {
  # Check if input is a list of matrices
  if (!is.list(coefficients) || any(!sapply(coefficients, is.matrix))) {
    stop("The input should be a list of matrices.")
  }

  # Draw heatmap function
  draw_heatmap <- function(data, title, colors) {
    data_melt <- reshape2::melt(data)
    colnames(data_melt) <- c("Row", "Column", "Value")

    plot <- ggplot2::ggplot(data = data_melt, ggplot2::aes(x = .data$Column, y = .data$Row, fill = .data$Value)) +
      ggplot2::geom_tile(color = "white", size = 0.5) +
      ggplot2::geom_text(aes(label = .data$Value), color = "black", size = 3) +
      ggplot2::scale_fill_gradientn(colors = colors, limits = c(0, max(data_melt$.data$Value))) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::labs(title = title, fill = NULL) +
      ggplot2::coord_fixed(ratio = 1) # Adjust aspect ratio to make the heatmap more square

    return(plot)
  }

  # Draw heatmaps
  hmap_overlap_features <- draw_heatmap(coefficients$overlap, 'Overlap Coefficient', RColorBrewer::brewer.pal(5, 'Oranges'))
  hmap_j_features <- draw_heatmap(coefficients$jaccard, 'Jaccard Coefficient', RColorBrewer::brewer.pal(5, 'Greens'))
  hmap_s_features <- draw_heatmap(coefficients$soerensen, 'Soerensen-Dice Coefficient', RColorBrewer::brewer.pal(5, 'Purples'))

  # Arrange heatmaps in one figure
  arranged_plot <- cowplot::plot_grid(hmap_overlap_features, hmap_j_features, hmap_s_features, nrow = 1, ncol = 3, align = 'h', rel_heights = c(1, 1, 1))
  if (save_plot) {
    ggplot2::ggsave(filename = filename, plot = arranged_plot)
  }

  return(arranged_plot)
}
