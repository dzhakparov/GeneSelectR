#' @title Compare gene lists obtained from the PipelineResults object
#' @description This function takes a PipelineResults object and creates heatmaps to visualize the overlap between the gene lists obtained
#' from different feature selection methods. It calculates the Overlap, Jaccard, and Soerensen-Dice coefficients to quantify
#' the similarity between gene lists.
#' @param input_list A PipelineResults object containing the fitted pipelines, cross-validation results, selected features,
#'   mean performance, and mean feature importances.
#' @param save_plot A logical value indicating whether to save the heatmap plots to a file or not. Default is FALSE.
#' @param filename A character string specifying the filename for the saved heatmap plots (if save_plot = TRUE).
#' @return A grid of heatmaps showing the Overlap, Jaccard, and Soerensen-Dice coefficients for the gene lists.
#' @examples
#' # Compare gene lists from the PipelineResults object obtained from fit_and_evaluate_pipelines
#' data(iris)
#' X <- iris[,1:4]
#' y <- iris[,5]
#' pipeline_results <- fit_and_evaluate_pipelines(X_train = X, y_train = y)
#' compare_gene_lists(input_list = pipeline_results)
#' importFrom GeneOverlap modOverlaps
#' @importFrom tmod modOverlaps
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradientn theme_minimal theme labs coord_fixed
#' @importFrom reshape2 melt
#' @importFrom RColorBrewer brewer.pal
#' @importFrom cowplot plot_grid
#' @export
compare_gene_lists <- function(input_list, save_plot = FALSE, filename = NULL, ...) {
  # Check if input object belongs to the PipelineResults class
  if (!inherits(input_list, "PipelineResults")) {
    stop("The input object does not belong to the PipelineResults class.")
  }

  # Create gene lists
  gene.lists <- lapply(input_list$mean_feature_importances, function(df) df$feature)
  names(gene.lists) <- names(input_list$mean_feature_importances)
  print(gene.lists)
  # Calculate the overlap coefficients and round to 2 decimal places
  calculate_coefficients <- function(stat) {
    coefficients <- tmod::modOverlaps(modules = gene.lists, mset = NULL, stat = stat)
    round(coefficients, 2)
  }

  overlap.coef_genes <- calculate_coefficients("overlap")
  j.coef_genes <- calculate_coefficients("jaccard")
  s.coef_genes <- calculate_coefficients("soerensen")
  print(overlap.coef_genes)
  print(j.coef_genes)
  print(s.coef_genes)

  # Draw heatmap function
  draw_heatmap <- function(data, title, colors) {
    data_melt <- reshape2::melt(data)
    colnames(data_melt) <- c("Row", "Column", "Value")

    plot <- ggplot2::ggplot(data = data_melt, aes(x = Column, y = Row, fill = Value)) +
      ggplot2::geom_tile(color = "white", size = 0.5) +
      ggplot2::geom_text(aes(label = Value), color = "black", size = 3) +
      ggplot2::scale_fill_gradientn(colors = colors, limits = c(0, max(data_melt$Value))) +
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
  hmap_overlap_genes <- draw_heatmap(overlap.coef_genes, 'Overlap Coefficient', RColorBrewer::brewer.pal(5, 'Oranges'))
  hmap_j_genes <- draw_heatmap(j.coef_genes, 'Jaccard Coefficient', RColorBrewer::brewer.pal(5, 'Greens'))
  hmap_s_genes <- draw_heatmap(s.coef_genes, 'Soerensen-Dice Coefficient', RColorBrewer::brewer.pal(5, 'Purples'))

  # Arrange heatmaps in one figure
  arranged_plot <- cowplot::plot_grid(hmap_overlap_genes, hmap_j_genes, hmap_s_genes, nrow = 1, ncol = 3, align = 'h', rel_heights = c(1, 1, 1))
  if (save_plot) {
    ggplot2::ggsave(filename = filename, plot = arranged_plot)
  }

  return(arranged_plot)
}
