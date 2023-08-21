library(testthat)
library(GeneSelectR)

# Load the UrbanRandomSubset object from the fixtures subfolder
load(testthat::test_path("fixtures", "UrbanRandomSubset.rda"))

# # Define a test
test_that("compare_gene_lists returns expected output", {
  GeneSelectR::skip_if_no_modules(names(import_python_packages()))
  X <- UrbanRandomSubset %>% select(-treatment)
  y <- UrbanRandomSubset %>% select(treatment)

  selection_results <- GeneSelectR(X,
                                   y,
                                   njobs = -1L,
                                   n_splits = 2L,
                                   max_features = 50L,
                                   calculate_permutation_importance = FALSE)

  # Call the function

  # Check that the output is a PipelineResults object
  expect_s4_class(results, "PipelineResults")

  # Check that the list has the expected elements
  expect_named(selection_results, c("best_pipeline", "cv_results", "inbuilt_feature_importance", 'test_metrics','cv_mean_score'))
})
