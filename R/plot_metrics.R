#' @title Plot Performance Metrics
#' @description This function creates separate plots for f1_mean, recall_mean, precision_mean, and accuracy_mean from a given dataframe, then arranges these plots using cowplot::plot_grid. Requires ggplot2 and cowplot packages.
#' @param pipeline_results An object of class "PipelineResults" containing the performance metrics. This object is expected to contain a dataframe with the columns 'method', 'f1_mean', 'f1_sd', 'recall_mean', 'recall_sd', 'precision_mean', 'precision_sd', 'accuracy_mean', 'accuracy_sd'.
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar labs theme_minimal
#' @importFrom cowplot plot_grid
#' @importFrom rlang .data
#' @return A combined ggplot object displaying the performance metrics.
#'         - If test metrics are provided, it includes separate plots for F1 mean, recall mean, precision mean, and accuracy mean, along with cross-validation mean score, arranged in a grid layout.
#'         - If no test metrics are available, it returns only the cross-validation mean score plot.
#' @examples
#'
#' # Assuming `pipeline_results` is a PipelineResults object with test metrics and CV mean score
#' pipeline_results <- new("PipelineResults",
#'                         test_metrics = data.frame(
#'                         method = c("Method1", "Method2"),
#'                         f1_mean = c(0.8, 0.85), f1_sd = c(0.05, 0.04),
#'                         recall_mean = c(0.75, 0.78), recall_sd = c(0.06, 0.05),
#'                         precision_mean = c(0.85, 0.88), precision_sd = c(0.05, 0.04),
#'                         accuracy_mean = c(0.9, 0.92), accuracy_sd = c(0.03, 0.02)),
#'                         cv_mean_score = data.frame(
#'                         method = c("Method1", "Method2"),
#'                         mean_score = c(0.88, 0.9), sd_score = c(0.02, 0.02)))
#'
#' # Plot the performance metrics
#' metric_plots <- plot_metrics(pipeline_results)
#' print(metric_plots)
#'
#'
#' @export
plot_metrics <- function(pipeline_results) {
  # Check if the input object is of the correct class
  if (!inherits(pipeline_results, "PipelineResults")) {
    stop("Input must be an object of class 'PipelineResults'")
  }

  # Extract the test_metrics dataframe from the object
  if (length(pipeline_results@test_metrics) == 0) {
    message('Test metrics weren"t calculated')
  } else {
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
  }

  cv_mean_score_df <- pipeline_results@cv_mean_score
  cv_plot <- ggplot2::ggplot(cv_mean_score_df, ggplot2::aes(x = .data$method, y = .data$mean_score)) +
    ggplot2::geom_col(fill = "darkblue", alpha = 0.8) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$mean_score - .data$sd_score, ymax = .data$mean_score + .data$sd_score), width = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "CV Mean Score", y = "Mean")
  if (length(pipeline_results@test_metrics) == 0) {
    message('No test set metrics provided. Only CV performance will be plotted')
  } else {
    #Combine the plots and share the legend
    combined_plot <- cowplot::plot_grid(cv_plot + theme(legend.position = "none"),
                                        f1_plot + theme(legend.position = "none"),
                                        recall_plot + theme(legend.position = "none"),
                                        precision_plot + theme(legend.position = "none"),
                                        accuracy_plot,
                                        labels = c("A", "B", "C", "D", "E"),
                                        nrow = 5,
                                        align = 'v',
                                        rel_heights = c(1,1,1,1,1.2))
    }

  return(if (length(pipeline_results@test_metrics) == 0) cv_plot else combined_plot)
}



