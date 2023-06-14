#' @title Get Feature Importances
#' @description This function extracts feature importances from a Scikit-learn pipeline
#' that has a Gradient Boosting Classifier as the final step.
#' @param pipeline A Scikit-learn pipeline object with a Gradient Boosting Classifier
#'                 as the final step.
#' @param X_train A DataFrame containing the training data.
#' @return A list containing the selected feature names and their importances, or NULL
#'         if the classifier is not a Gradient Boosting Classifier or the feature selector
#'         doesn't have the 'get_support' method.
#' @importFrom reticulate py_has_attr py_to_r
#' @examples
#' \dontrun{
#' # Assuming you have a Scikit-learn pipeline 'my_pipeline' and training data 'X_train'
#' feature_importances <- get_feature_importances(my_pipeline, X_train)
#' # Extract the selected feature names and their importances
#' selected_features <- feature_importances$selected_features
#' importances <- feature_importances$importances
#' }
#' @export
get_feature_importances <- function(pipeline, X_train) {
  classifier <- pipeline$named_steps[['classifier']]

  if (reticulate::py_has_attr(classifier, "coef_")) {
    feature_importances <- classifier$coef_
  } else if (reticulate::py_has_attr(classifier, "feature_importances_")) {
    feature_importances <- classifier$feature_importances_
  } else {
    cat("Classifier doesn't have coef_ or feature_importances_ attributes")
    return(NULL)
  }

  feature_selector <- pipeline$named_steps[["feature_selector"]]
  original_feature_names <- colnames(reticulate::py_to_r(X_train))

  # Check if the feature selector has the get_support method
  if (reticulate::py_has_attr(feature_selector, "get_support")) {
    selected_indices <- which(feature_selector$get_support())
    selected_feature_names <- original_feature_names[selected_indices]

    importances <- data.frame(feature=selected_feature_names, importance=feature_importances)
    importances <- importances[order(-importances$importance),]
    return(importances)
  }
  else if (reticulate::py_has_attr(feature_selector, "support_")) {
    selected_indices <- which(feature_selector$support_)
    selected_feature_names <- original_feature_names[selected_indices]

    importances <- data.frame(feature=selected_feature_names, importance=feature_importances)
    importances <- importances[order(-importances$importance),]
    return(importances)
  } else {
    cat("Feature selector doesn't have get_support() attribute")
  }
  return(NULL)
}

#' @title Create Pipelines
#' @description This function creates a list of Scikit-learn pipelines using the specified feature selection methods, preprocessing steps, and classifier.
#' @param feature_selection_methods A list of feature selection methods to use for the pipelines.
#' @param preprocessing_steps A list of preprocessing steps to use for the pipelines.
#' @param selected_methods A vector of names of feature selection methods to use from the default set.
#' @param classifier A Scikit-learn classifier to use as the final step in the pipelines.
#' @return A list of Scikit-learn pipelines.
#' @importFrom reticulate import tuple
#' @export
create_pipelines <- function(feature_selection_methods, preprocessing_steps, selected_methods, classifier) {
  sklearn <- reticulate::import('sklearn')
  pipeline <- sklearn$pipeline$Pipeline
  named_pipelines <- list()
  selected_methods <- names(feature_selection_methods)


  for (feature_selector_name in selected_methods) {
    if (feature_selector_name %in% names(feature_selection_methods)) {
      feature_selector_method <- feature_selection_methods[[feature_selector_name]]
      base_model <- classifier

      steps <- c(preprocessing_steps, list("feature_selector" =feature_selector_method))
      steps <- c(steps, list("classifier" = base_model))

      tuple_steps <- steps_to_tuples(steps)

      named_pipelines[[feature_selector_name]] <- pipeline(steps = tuple_steps)
    } else {
      cat("Warning: Feature selection method", feature_selector_name, "not found.\n")
    }
  }

  return(named_pipelines)
}

#' @title Convert Steps to Tuples
#' @description This function converts a list of steps to tuples for use in a Scikit-learn pipeline.
#' @param steps A list of steps to convert to tuples.
#' @return A list of tuples.
#' @importFrom reticulate tuple
#' @export
steps_to_tuples <- function(steps) {
  tuple_steps <- c()
  for (step_name in names(steps)) {
    step_obj <- steps[[step_name]]
    tuple_steps <- c(tuple_steps, reticulate::tuple(step_name, step_obj))
  }
  return(tuple_steps)
}


#' Aggregate Feature Importances
#'
#' This function aggregates the feature importances for each method across all splits.
#'
#' @param selected_features A list of selected features. Each element of the list represents a split and should be a named list where the names are the methods and the values are data frames containing the feature importances for that method in that split.
#'
#' @return A list of aggregated feature importances for each method. Each element of the list is a data frame that contains the mean and standard deviation of the feature importances for a particular method across all splits.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarize filter
#' @importFrom stats sd
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#'   # Assuming selected_features is a list of selected features for each split
#'   aggregated_importances <- aggregate_feature_importances(selected_features)
#' }
#'
aggregate_feature_importances <- function(selected_features) {
  aggregated_importances <- list()

  # Assuming the first split has the same methods as the others
  for (method in names(selected_features[[1]])) {
    # Extract feature importances for the current method across all splits
    feature_importances <- lapply(selected_features, function(split) {
      as.data.frame(split[[method]])
    })

    # Combine the feature importances from all splits
    combined_importances <- do.call(rbind, feature_importances)

    importances_df <- combined_importances %>%
      dplyr::group_by(.data$feature) %>%
      dplyr::summarize(mean_importance = mean(.data$importance, na.rm = TRUE),
                       std = stats::sd(.data$importance, na.rm = TRUE))

    importances_df <- importances_df %>%
      dplyr::filter(.data$mean_importance > 0)

    # Add the aggregated importances for the current method to the results list
    aggregated_importances[[method]] <- importances_df
  }

  return(aggregated_importances)
}

