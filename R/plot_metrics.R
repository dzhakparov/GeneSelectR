#' @title Plot Performance Metrics
#' @description This function creates separate plots for f1_mean, recall_mean, precision_mean, and accuracy_mean from a given dataframe, then arranges these plots using cowplot::plot_grid. Requires ggplot2 and cowplot packages.
#' @param pipeline_results An object of class "PipelineResults" containing the performance metrics. This object is expected to contain a dataframe with the columns 'method', 'f1_mean', 'f1_sd', 'recall_mean', 'recall_sd', 'precision_mean', 'precision_sd', 'accuracy_mean', 'accuracy_sd'.
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar labs theme_minimal
#' @importFrom cowplot plot_grid
#' @importFrom rlang .data
#' @return A combined ggplot object
#' @export
plot_metrics <- function(pipeline_results) {
  # Check if the input object is of the correct class
  if (!inherits(pipeline_results, "PipelineResults")) {
    stop("Input must be an object of class 'PipelineResults'")
  }

  # Extract the test_metrics dataframe from the object
  test_metrics_df <- pipeline_results@test_metrics

  # Define the plots for each metric
  f1_plot <- ggplot2::ggplot(test_metrics_df, ggplot2::aes(x = .data$method, y = .data$f1_mean)) +
    ggplot2::geom_col(fill = "skyblue", alpha = 0.8) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$f1_mean - .data$f1_sd, ymax = .data$f1_mean + .data$f1_sd), width = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "F1 Score", y = "Mean")

  recall_plot <- ggplot2::ggplot(test_metrics_df, ggplot2::aes(x = .data$method, y = .data$recall_mean)) +
    ggplot2::geom_col(fill = "lightgreen", alpha = 0.8) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$recall_mean - .data$recall_sd, ymax = .data$recall_mean + .data$recall_sd), width = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Recall", y = "Mean")

  precision_plot <- ggplot2::ggplot(test_metrics_df, ggplot2::aes(x = .data$method, y = .data$precision_mean)) +
    ggplot2::geom_col(fill = "coral", alpha = 0.8) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$precision_mean - .data$precision_sd, ymax = .data$precision_mean + .data$precision_sd), width = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Precision", y = "Mean")

  accuracy_plot <- ggplot2::ggplot(test_metrics_df, ggplot2::aes(x = .data$method, y = .data$accuracy_mean)) +
    ggplot2::geom_col(fill = "purple", alpha = 0.8) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$accuracy_mean - .data$accuracy_sd, ymax = .data$accuracy_mean + .data$accuracy_sd), width = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Accuracy", y = "Mean")

  # Combine the plots and share the legend
  combined_plot <- cowplot::plot_grid(f1_plot + theme(legend.position = "none"),
                                      recall_plot + theme(legend.position = "none"),
                                      precision_plot + theme(legend.position = "none"),
                                      accuracy_plot,
                                      labels = c("A", "B", "C", "D"),
                                      nrow = 4,
                                      align = 'v',
                                      rel_heights = c(1,1,1,1.2))

  return(combined_plot)
}



