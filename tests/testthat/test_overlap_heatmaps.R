testthat::expect_warning()

# Create a mock list of matrices for testing
mock_coefficients <- list(
  inbuilt_feature_importance_coefficient = list(
    overlap = matrix(1:4, nrow = 2),
    jaccard = matrix(1:4, nrow = 2),
    soerensen = matrix(1:4, nrow = 2)
  ),
  permutation_importance_coefficients = list(
    overlap = matrix(1:4, nrow = 2),
    jaccard = matrix(1:4, nrow = 2),
    soerensen = matrix(1:4, nrow = 2)
  )
)

# Test 1: Check if the function returns a ggplot object
test_that("plot_overlap_heatmaps returns a ggplot object", {
  plot <- plot_overlap_heatmaps(mock_coefficients)
  expect_s3_class(plot, "gg")
})

# Test 2: Check if it handles the absence of permutation_importance_coefficients correctly
test_that("Handles absence of permutation_importance_coefficients correctly", {
  mock_coefficients_no_perm <- mock_coefficients
  mock_coefficients_no_perm$permutation_importance_coefficients <- NULL
  plot <- plot_overlap_heatmaps(mock_coefficients_no_perm)
  expect_s3_class(plot, "gg")
})

# Test 3: Check if it throws an error for incorrect input types
test_that("Throws an error for incorrect input types", {
  expect_error(plot_overlap_heatmaps(list(a = 1)), "The input should be a list of lists, each containing matrices.")
})
