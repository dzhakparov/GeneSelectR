#' @title Get Feature Importances
#' @description This function extracts feature importances from a Scikit-learn pipeline
#' that has a Gradient Boosting Classifier as the final step.
#' @param pipeline A Scikit-learn pipeline object with a Gradient Boosting Classifier
#'                 as the final step.
#' @param X_train A DataFrame containing the training data.
#' @param pipeline_name Strings (names of the selected_pipelines list) representing pipeline names that were constructed for the feature selection
#' @param iter An integer that is indicating current iteration of the train-test split
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
get_feature_importances <- function(pipeline, X_train, pipeline_name, iter) {
  classifier <- pipeline$named_steps[['classifier']]

  if (reticulate::py_has_attr(classifier, "coef_")) {
    feature_importances <- classifier$coef_
    if (dim(feature_importances)[1] == 1) {
      feature_importances <- feature_importances[1,]
    }
  } else if (reticulate::py_has_attr(classifier, "feature_importances_")) {
    feature_importances <- classifier$feature_importances_
  } else {
    cat("Classifier doesn't have coef_ or feature_importances_ attributes")
    return(NULL)
  }

  feature_selector <- pipeline$named_steps[["feature_selector"]]
  original_feature_names <- colnames(reticulate::py_to_r(X_train))

  if (reticulate::py_has_attr(feature_selector, "get_support")) {
    selected_indices <- which(feature_selector$get_support())
  } else if (reticulate::py_has_attr(feature_selector, "support_")) {
    selected_indices <- which(feature_selector$support_)
  } else {
    cat("Feature selector doesn't have get_support() or support_ attribute")
    return(NULL)
}

  selected_feature_names <- original_feature_names[selected_indices]
  importances <- data.frame(feature=selected_feature_names, importance=feature_importances[selected_indices])
  importances <- importances[order(-importances$importance),]
  importances$rank <- seq_len(nrow(importances))
  column_name <- as.character(glue::glue('rank_{pipeline_name}_split_{iter}'))
  colnames(importances)[colnames(importances) == 'rank'] <- column_name

  return(importances)
}

# get_feature_importances <- function(pipeline, X_train, pipeline_name, iter) {
#   classifier <- pipeline$named_steps[['classifier']]
#
#   if (reticulate::py_has_attr(classifier, "coef_")) {
#     feature_importances <- classifier$coef_
#   } else if (reticulate::py_has_attr(classifier, "feature_importances_")) {
#     feature_importances <- classifier$feature_importances_
#   } else {
#     cat("Classifier doesn't have coef_ or feature_importances_ attributes")
#     return(NULL)
#   }
#
#   feature_selector <- pipeline$named_steps[["feature_selector"]]
#   original_feature_names <- colnames(reticulate::py_to_r(X_train))
#   # Check if the feature selector has the get_support method
#   if (reticulate::py_has_attr(feature_selector, "get_support")) {
#     selected_indices <- which(feature_selector$get_support())
#     selected_feature_names <- original_feature_names[selected_indices]
#     importances <- data.frame(feature=selected_feature_names, importance=feature_importances)
#     print(importances)
#     importances <- importances[order(-importances$importance),]
#     importances$rank <- seq_len(nrow(importances))
#     column_name <- as.character(glue::glue('rank_{pipeline_name}_split_{iter}'))
#     colnames(importances)[colnames(importances) == 'rank'] <- column_name
#     return(importances)
#   }
#   else if (reticulate::py_has_attr(feature_selector, "support_")) {
#     selected_indices <- which(feature_selector$support_)
#     selected_feature_names <- original_feature_names[selected_indices]
#
#     importances <- data.frame(feature=selected_feature_names, importance=feature_importances)
#     importances <- importances[order(-importances$importance),]
#     importances$rank <- seq_len(nrow(importances))
#     column_name <- as.character(glue::glue('rank_{pipeline_name}_split_{iter}'))
#     colnames(importances)[colnames(importances) == 'rank'] <- column_name
#     return(importances)
#   } else {
#     cat("Feature selector doesn't have get_support() attribute")
#   }
#   return(NULL)
#
# }

#' @title Calculate Permutation Feature Importance
#' @description This function calculates permutation feature importance for a Scikit-learn
#' pipeline with a trained classifier as the final step.
#' @param pipeline A Scikit-learn pipeline object with a trained classifier as the final step.
#' @param X_train A DataFrame containing the training data.
#' @param y_train A DataFrame containing the training labels.
#' @param n_repeats An integer specifying the number of times to permute each feature.
#' @param random_state An integer specifying the seed for the random number generator.
#' @param njobs An integer specifying number of cores to use. Set up by the master GeneSelectR function.
#' @param pipeline_name Strings (names of the selected_pipelines list) representing pipeline names that were constructed for the feature selection
#' @param iter An integer that is indicating current iteration of the train-test split
#' @return A list containing the feature names and their importance scores.
#' @importFrom reticulate import py_to_r
#' @examples
#' \dontrun{
#' # Assuming you have a Scikit-learn pipeline 'my_pipeline' and training data 'X_train'
#' permutation_importances <- calculate_permutation_feature_importance(my_pipeline, X_train, y_train)
#' }
#' @export

calculate_permutation_feature_importance <- function(pipeline,
                                                     X_train,
                                                     y_train,
                                                     n_repeats=10L,
                                                     random_state=0L,
                                                     njobs = njobs,
                                                     pipeline_name,
                                                     iter) {
  # Import the required function
  permutation_importance <- reticulate::import("sklearn.inspection", convert = FALSE)$permutation_importance

  # Compute the permutation feature importance
  perm_importance <- permutation_importance(pipeline, X_train, y_train, n_repeats = n_repeats, random_state = random_state, n_jobs = njobs)

  # Extract the importances and feature names
  importances <- reticulate::py_to_r(perm_importance$importances_mean)
  feature_names <- colnames(reticulate::py_to_r(X_train))

  # Create a data frame
  importance_df <- data.frame(feature=feature_names, importance=importances)
  importance_df <- importance_df[order(-importance_df$importance),]

  # Calculate the rank of the feature importances
  importance_df$rank <- seq_len(nrow(importance_df))
  # Get the pipeline name and append it to the rank column name
  column_name <- as.character(glue::glue('rank_{pipeline_name}_split_{iter}'))
  colnames(importance_df)[colnames(importance_df) == 'rank'] <- column_name


  return(importance_df)
}


#' @title Create Pipelines
#' @description This function creates a list of Scikit-learn pipelines using the specified feature selection methods, preprocessing steps, and classifier.
#' @param feature_selection_methods A list of feature selection methods to use for the pipelines.
#' @param preprocessing_steps A list of preprocessing steps to use for the pipelines.
#' @param selected_methods A vector of names of feature selection methods to use from the default set.
#' @param classifier A Scikit-learn classifier to use as the final step in the pipelines.
#' @param fs_param_grids param grid
#' @return A list of Scikit-learn pipelines.
#' @importFrom reticulate import tuple py_bool
create_pipelines <- function(feature_selection_methods, preprocessing_steps, selected_methods, classifier, fs_param_grids) {
  sklearn <- reticulate::import('sklearn')
  pipeline <- sklearn$pipeline$Pipeline
  named_pipelines <- list()
  selected_methods <- names(feature_selection_methods)

  for (feature_selector_name in selected_methods) {
    if (feature_selector_name %in% names(feature_selection_methods)) {
      feature_selector_method <- feature_selection_methods[[feature_selector_name]]
      base_model <- classifier

      steps <- c(preprocessing_steps, list("feature_selector" = feature_selector_method))
      steps <- c(steps, list("classifier" = base_model))

      tuple_steps <- steps_to_tuples(steps)

      # Add feature selection parameters to the pipeline if they are provided
      if (feature_selector_name %in% names(fs_param_grids)) {
        fs_params <- fs_param_grids[[feature_selector_name]]

        # Incorporate the parameters from fs_params into the appropriate estimator objects
        for (i in seq_along(tuple_steps)) {
          if (reticulate::py_bool(tuple_steps[[i]][[1]] == "feature_selector")) {
            tuple_steps[[i]][[2]] <- do.call(tuple_steps[[i]][[2]], fs_params)
          }
        }
      }

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

  for (method in names(selected_features[[1]])) {
    feature_importances <- lapply(selected_features, function(split) {
      # Reshape the data from wide format to long format
      split_df <- tidyr::pivot_longer(split[[method]],
                                      cols = starts_with("rank_"),
                                      names_to = "method",
                                      values_to = "rank")
      as.data.frame(split_df)
    })

    combined_importances <- do.call(rbind, feature_importances)

    importances_df <- combined_importances %>%
      dplyr::group_by(.data$feature) %>%
      dplyr::summarize(mean_importance = mean(.data$importance, na.rm = TRUE),
                       std = stats::sd(.data$importance, na.rm = TRUE))

    # Add rank columns back
    rank_df <- combined_importances %>%
      dplyr::select(.data$feature, .data$method, .data$rank)

    # Join importances_df with rank_df
    importances_df <- dplyr::left_join(importances_df, rank_df, by = "feature")

    # Reshape back to wide format
    importances_df <- tidyr::pivot_wider(importances_df,
                                         names_from = method,
                                         values_from = rank)
    importances_df <- importances_df %>%
      dplyr::filter(.data$mean_importance > 0)

    # Add the aggregated importances for the current method to the results list
    aggregated_importances[[method]] <- importances_df
  }

  return(aggregated_importances)
}

#' Save HTML Representation of a GridSearchCV Object
#'
#' This function takes a GridSearchCV object from scikit-learn and saves its HTML representation to a specified directory.
#'
# @param grid_search A GridSearchCV object from scikit-learn.
# @param output_dir A string specifying the directory where the HTML file will be saved.
# @return A message indicating the location where the HTML file has been saved.
# @examples
# \dontrun{
# library(reticulate)
# # Assuming 'grid_search' is your GridSearchCV object
# save_pipeline_html(grid_search, "path/to/output/directory")
# }
# @importFrom glue glue

# save_pipeline_html <- function(grid_search, filename, output_dir) {
#   # Import necessary Python modules
#   #sklearn <- import('sklearn')
#   sklearn$set_config('diagram') # Enable HTML representation
#
#   # Convert the GridSearchCV object to HTML
#   print(grid_search)
#
#   # Create the output directory if it doesn't exist
#   if (!dir.exists(output_dir)) {
#     dir.create(output_dir)
#   }
#
#   # Define the output file path
#   output_file <- file.path(glue::glue('{output_dir}/{filename}_pipeline.html'))
#
#   # Save HTML to the file
#   writeLines(print(grid_search), output_file)
#
#   cat("HTML representation saved to", output_file, "\n")
# }
