# Initiate conda env
GeneSelectR::configure_environment()
GeneSelectR::set_reticulate_python()
library(dplyr)
data("UrbanRandomSubset")

X <- UrbanRandomSubset %>% dplyr::select(-treatment)
y <- UrbanRandomSubset %>% dplyr::select(treatment)

selection_results <- GeneSelectR::GeneSelectR(X,
                                              y,
                                              max_features = 20L,
                                              calculate_permutation_importance = TRUE)
plot_metrics(selection_results)
coeffs <- calculate_overlap_coefficients(selection_results)
generate_overlap_heatmaps(coeffs)

# Simpler version of the heatmap function for debugging
draw_heatmap <- function(data) {
  data_melt <- reshape2::melt(data)
  colnames(data_melt) <- c("Row", "Column", "Value")

  # Print the head of the data frame
  print(head(data_melt))

  plot <- ggplot2::ggplot(data = data_melt, ggplot2::aes(x = Row, y = Column, fill = Value)) +
    ggplot2::geom_tile(color = "white", size = 0.5) +
    ggplot2::geom_text(aes(label = Value), color = "black", size = 3) +
    ggplot2::scale_fill_gradientn(colors = RColorBrewer::brewer.pal(5, 'Oranges'),
                                  limits = c(min(data_melt$Value, na.rm = TRUE),
                                             max(data_melt$Value, na.rm = TRUE))) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(title = 'Overlap Coefficient', fill = NULL) +
    ggplot2::coord_fixed(ratio = 1) # Adjust aspect ratio to make the heatmap more square
  print(plot)
}

# Test the function with one of your matrices
draw_heatmap(coeffs$feature_importance_coefficients$overlap)

