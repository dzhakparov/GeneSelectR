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



set.seed(123) # for reproducibility
n_rows <- 10
n_features <- 100

# Randomly generate feature data
X <- as.data.frame(matrix(rnorm(n_rows * n_features), nrow = n_rows, ncol = n_features))
# Ensure each feature has a variance greater than 0.85
for(i in 1:ncol(X)) {
 while(var(X[[i]]) <= 0.85) {
   X[[i]] <- X[[i]] * 1.1
 }
}
colnames(X) <- paste0("Feature", 1:n_features)

# Create a mock binary label column
y <- factor(sample(c("Class1", "Class2"), n_rows, replace = TRUE))

# set up the environment
GeneSelectR::configure_environment()
GeneSelectR::set_reticulate_python()

# run GeneSelectR
results <- GeneSelectR(X, y)


# Perform gene selection and evaluation using user-defined methods
fs_methods <- list("Lasso" = select_model(lasso(penalty = 'l1',
                                                C = 0.1,
                                                solver = 'saga'),
                                          threshold = 'median'))
custom_fs_grids <- list("Lasso" = list('C' = c(0.1, 1, 10)))
results <- GeneSelectR(X,
                       y,
                       max_features = 15,
                       custom_fs_methods = fs_methods,
                       custom_fs_grids = custom_fs_grids)

#' # Simple Usage with Mock Data
#' # Create a mock PipelineResults object with minimal data
mock_pipeline_results <- new("PipelineResults",
                             inbuilt_feature_importance = list(
                             "GeneSet1" = data.frame(feature = c("BRCA1", "TP53"))),
                             permutation_importance = list(
                               "GeneSet1" = data.frame(feature = c("BRCA1", "TP53"))))

# Mock annotations data frame
mock_annotations_ahb <- data.frame(gene_id = c("BRCA1", "TP53"),
                                   gene_name = c("BRCA1", "TP53"),
                                   entrezid = c(101, 102))

# Convert and annotate gene lists
annotated_lists <- annotate_gene_lists(mock_pipeline_results,
                                       custom_lists = NULL,
                                       mock_annotations_ahb,
                                       "SYMBOL")
print(annotated_lists)

# Using Custom Gene Lists
# Create custom gene lists
custom_gene_lists <- list("CustomList1" = c("BRCA1", "TP53"))

# Convert and annotate gene lists with custom gene lists included
annotated_lists_custom <- annotate_gene_lists(mock_pipeline_results,
                                              custom_gene_lists,
                                              mock_annotations_ahb,
                                              "SYMBOL")
print(annotated_lists_custom)



