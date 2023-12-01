# Load the PipelineResults object from the fixture
pipeline_results_fixture <- readRDS("./fixtures/PipelineResults.rds")

# Test 1: Check if the function returns a ggplot object
test_that("plot_metrics returns a ggplot object", {
  plot <- plot_metrics(pipeline_results_fixture)
  expect_s3_class(plot, "gg")
})

# Test 2: Check if it handles the absence of test_metrics correctly
test_that("Handles absence of test_metrics correctly", {
  pipeline_results_no_test_metrics <- pipeline_results_fixture
  pipeline_results_no_test_metrics@test_metrics <- list()
  plot <- plot_metrics(pipeline_results_no_test_metrics)
  expect_s3_class(plot, "gg")
})

# Test 3: Check if it throws an error for incorrect input types
test_that("Throws an error for incorrect input types", {
  expect_error(plot_metrics(data.frame()), "Input must be an object of class 'PipelineResults'")
})
