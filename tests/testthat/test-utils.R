# =============================================================================
# test-utils.R — Unit tests for GeneSelectR utility functions
# =============================================================================

# ---------------------------------------------------------------------------
# percentile01
# ---------------------------------------------------------------------------
test_that("percentile01 normalises to (0,1]", {
  x <- c(0, 1, 2, 3, 4, 5)
  out <- percentile01(x)

  expect_length(out, length(x))
  expect_true(all(out >= 0 & out <= 1))
  expect_equal(out[1], 0)
  expect_true(all(diff(out[-1]) >= 0))
})

test_that("percentile01 handles all-zero input", {
  expect_equal(percentile01(rep(0, 5)), rep(0, 5))
})

test_that("percentile01 handles single unique value", {
  expect_equal(percentile01(rep(3, 4)), rep(0, 4))
})

test_that("percentile01 handles NA values gracefully", {
  x <- c(1, 2, NA, 4)
  out <- percentile01(x)
  expect_length(out, 4)
  expect_equal(out[3], 0)
})

test_that("percentile01 handles ties correctly", {
  x <- c(0, 2, 2, 5)
  out <- percentile01(x)
  expect_equal(out[2], out[3])
})

# ---------------------------------------------------------------------------
# validate_inputs
# ---------------------------------------------------------------------------
test_that("validate_inputs passes with valid data", {
  X <- matrix(rnorm(200), nrow = 20, ncol = 10)
  colnames(X) <- paste0("Gene", 1:10)
  y <- factor(rep(c("A", "B"), each = 10))
  expect_true(validate_inputs(X, y, colnames(X), n_folds = 5))
})

test_that("validate_inputs rejects non-matrix X", {
  expect_error(
    validate_inputs(data.frame(a = 1:10), factor(rep("A", 10))),
    "X must be a matrix"
  )
})

test_that("validate_inputs rejects non-factor y", {
  X <- matrix(1, 10, 2, dimnames = list(NULL, c("A", "B")))
  expect_error(validate_inputs(X, c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)), "y must be a factor")
})

test_that("validate_inputs rejects multi-class y", {
  X <- matrix(1, 15, 2, dimnames = list(NULL, c("A", "B")))
  y <- factor(rep(c("A", "B", "C"), each = 5))
  expect_error(validate_inputs(X, y), "exactly 2 levels")
})

test_that("validate_inputs rejects dimension mismatch", {
  X <- matrix(1, 10, 2, dimnames = list(NULL, c("A", "B")))
  y <- factor(rep(c("A", "B"), each = 6))
  expect_error(validate_inputs(X, y), "mismatch")
})

test_that("validate_inputs rejects duplicate gene names", {
  X <- matrix(1, 10, 3, dimnames = list(NULL, c("G1", "G1", "G2")))
  y <- factor(rep(c("A", "B"), each = 5))
  expect_error(validate_inputs(X, y), "Duplicate")
})

test_that("validate_inputs rejects missing colnames", {
  X <- matrix(1, 10, 2)
  y <- factor(rep(c("A", "B"), each = 5))
  expect_error(validate_inputs(X, y), "column names")
})

test_that("validate_inputs warns about zero-variance genes", {
  X <- matrix(rnorm(200), 20, 10)
  X[, 3] <- 5
  colnames(X) <- paste0("G", 1:10)
  y <- factor(rep(c("A", "B"), each = 10))
  expect_warning(validate_inputs(X, y), "zero variance")
})

test_that("validate_inputs warns about NAs", {
  X <- matrix(rnorm(200), 20, 10)
  X[1, 1] <- NA
  colnames(X) <- paste0("G", 1:10)
  y <- factor(rep(c("A", "B"), each = 10))
  expect_warning(validate_inputs(X, y), "NA")
})

test_that("validate_inputs rejects infinite values", {
  X <- matrix(rnorm(200), 20, 10)
  X[1, 1] <- Inf
  colnames(X) <- paste0("G", 1:10)
  y <- factor(rep(c("A", "B"), each = 10))
  expect_error(validate_inputs(X, y), "infinite")
})

test_that("validate_inputs rejects too-few samples for CV", {
  X <- matrix(rnorm(16), 4, 4, dimnames = list(NULL, paste0("G", 1:4)))
  y <- factor(c("A", "A", "B", "B"))
  expect_error(validate_inputs(X, y, n_folds = 5), "Insufficient")
})

# ---------------------------------------------------------------------------
# adaptive_bin_count
# ---------------------------------------------------------------------------
test_that("adaptive_bin_count returns reasonable values", {
  expect_equal(adaptive_bin_count(100, 5), 5)
  expect_equal(adaptive_bin_count(9, 5), 3)
  expect_gte(adaptive_bin_count(50, 10), 3)
  expect_lte(adaptive_bin_count(50, 10), 10)
})

# ---------------------------------------------------------------------------
# discretize_continuous
# ---------------------------------------------------------------------------
test_that("discretize_continuous returns integer bins", {
  x <- rnorm(100)
  bins <- discretize_continuous(x, n_bins = 5)
  expect_type(bins, "integer")
  expect_length(bins, 100)
  expect_true(all(bins >= 0))
})

test_that("discretize_continuous handles constant input", {
  x <- rep(3, 50)
  bins <- discretize_continuous(x, n_bins = 5)
  expect_true(all(bins == 0))
})

# ---------------------------------------------------------------------------
# compute_mutual_information
# ---------------------------------------------------------------------------
test_that("MI is non-negative", {
  set.seed(1)
  x <- rnorm(100)
  y <- factor(rep(c(0, 1), each = 50))
  mi <- compute_mutual_information(x, y)
  expect_gte(mi, 0)
})

test_that("MI is higher for informative features", {
  set.seed(42)
  n <- 200
  y <- factor(rep(c(0, 1), each = n / 2))
  x_informative <- c(rnorm(n / 2, mean = 0), rnorm(n / 2, mean = 3))
  x_noise <- rnorm(n)

  mi_signal <- compute_mutual_information(x_informative, y)
  mi_noise  <- compute_mutual_information(x_noise, y)
  expect_gt(mi_signal, mi_noise)
})

test_that("MI is zero for constant feature", {
  x <- rep(1, 100)
  y <- factor(rep(c(0, 1), each = 50))
  mi <- compute_mutual_information(x, y)
  expect_equal(mi, 0)
})

test_that("compute_mi_vectorized returns correct length", {
  set.seed(1)
  X <- matrix(rnorm(500), 50, 10)
  y <- factor(rep(c(0, 1), each = 25))
  mi <- compute_mi_vectorized(X, y)
  expect_length(mi, 10)
  expect_true(all(mi >= 0))
})

# ---------------------------------------------------------------------------
# create_cv_folds
# ---------------------------------------------------------------------------
test_that("create_cv_folds creates correct number of folds", {
  y <- factor(rep(c("A", "B"), each = 50))
  folds <- create_cv_folds(y, K = 5, R = 3, random_seed = 1)
  expect_length(folds, 15)
})

test_that("create_cv_folds produces non-overlapping train/test", {
  y <- factor(rep(c("A", "B"), each = 50))
  folds <- create_cv_folds(y, K = 5, R = 1, random_seed = 1)

  for (fold in folds) {
    expect_equal(length(intersect(fold$train, fold$test)), 0)
    expect_equal(unname(sort(c(fold$train, fold$test))), 1:100)
  }
})

test_that("create_cv_folds is stratified", {
  y <- factor(rep(c("A", "B"), each = 50))
  folds <- create_cv_folds(y, K = 5, R = 1, random_seed = 1)

  for (fold in folds) {
    test_classes <- table(y[fold$test])
    expect_true(all(test_classes >= 8 & test_classes <= 12))
  }
})

test_that("create_cv_folds is reproducible", {
  y <- factor(rep(c("A", "B"), each = 50))
  folds1 <- create_cv_folds(y, K = 5, R = 2, random_seed = 42)
  folds2 <- create_cv_folds(y, K = 5, R = 2, random_seed = 42)
  expect_identical(folds1, folds2)
})

# ---------------------------------------------------------------------------
# combine_scores
# ---------------------------------------------------------------------------
test_that("arithmetic combination is a weighted mean", {
  pi <- c(0.8, 0.4, 0.1)
  u  <- c(0.6, 0.5, 0.3)
  b  <- c(0.9, 0.7, 0.2)
  w  <- c(1, 1, 1)
  out <- combine_scores(pi, u, b, formula = "arithmetic", weights = w)
  expected <- (pi + u + b) / 3
  expect_equal(out, expected, tolerance = 1e-10)
})

test_that("geometric combination respects rank order", {
  pi <- c(0.9, 0.5, 0.1)
  u  <- c(0.9, 0.5, 0.1)
  b  <- c(0.9, 0.5, 0.1)
  out <- combine_scores(pi, u, b, formula = "geometric")
  expect_true(out[1] > out[2])
  expect_true(out[2] > out[3])
})

test_that("harmonic combination penalises low components", {
  pi <- c(0.9, 0.9)
  u  <- c(0.9, 0.1)
  b  <- c(0.9, 0.9)
  out <- combine_scores(pi, u, b, formula = "harmonic")
  expect_gt(out[1], out[2])
})

test_that("minimum combination returns element-wise min", {
  pi <- c(0.8, 0.3)
  u  <- c(0.5, 0.9)
  b  <- c(0.6, 0.4)
  out <- combine_scores(pi, u, b, formula = "minimum")
  expect_equal(out, pmin(pi, u, b))
})

test_that("unknown formula raises error", {
  expect_error(combine_scores(1, 1, 1, formula = "unknown"), "Unknown formula")
})

test_that("custom weights change the ranking", {
  pi <- c(0.9, 0.2)
  u  <- c(0.1, 0.8)
  b  <- c(0.5, 0.5)

  out_pi_heavy <- combine_scores(pi, u, b, "arithmetic", weights = c(5, 1, 1))
  out_u_heavy  <- combine_scores(pi, u, b, "arithmetic", weights = c(1, 5, 1))

  expect_gt(out_pi_heavy[1], out_pi_heavy[2])
  expect_gt(out_u_heavy[2], out_u_heavy[1])
})

# ---------------------------------------------------------------------------
# aggregate_cv_results
# ---------------------------------------------------------------------------
test_that("aggregate_cv_results computes correct selection frequency", {
  n_genes <- 5

  cv_results <- list(
    list(selected = c(1, 3), coefficients = c(0.5, 0.3),
         mi_scores = c(0.1, 0.2, 0.3, 0.05, 0.15), auc = 0.8,
         fold_num = 1, repeat_num = 1),
    list(selected = c(1, 2, 3), coefficients = c(0.4, 0.2, 0.6),
         mi_scores = c(0.12, 0.22, 0.28, 0.06, 0.14), auc = 0.85,
         fold_num = 2, repeat_num = 1),
    list(selected = c(1), coefficients = c(0.7),
         mi_scores = c(0.11, 0.19, 0.31, 0.04, 0.16), auc = 0.75,
         fold_num = 3, repeat_num = 1),
    list(selected = c(1, 3, 5), coefficients = c(0.3, 0.5, 0.1),
         mi_scores = c(0.09, 0.21, 0.29, 0.07, 0.13), auc = 0.82,
         fold_num = 4, repeat_num = 1)
  )

  agg <- aggregate_cv_results(cv_results, n_genes)

  expect_equal(agg$pi_exact[1], 1.0)
  expect_equal(agg$pi_exact[4], 0.0)
  expect_equal(agg$pi_exact[3], 0.75)
  expect_length(agg$u_coef, n_genes)
  expect_length(agg$u_mi, n_genes)
})

test_that("aggregate_cv_results handles empty selections", {
  cv_results <- list(
    list(selected = integer(0), coefficients = numeric(0),
         mi_scores = c(0.1, 0.2), auc = 0.5, fold_num = 1, repeat_num = 1),
    list(selected = integer(0), coefficients = numeric(0),
         mi_scores = c(0.15, 0.25), auc = 0.5, fold_num = 2, repeat_num = 1)
  )

  agg <- aggregate_cv_results(cv_results, 2)
  expect_equal(agg$pi_exact, c(0, 0))
})

# ---------------------------------------------------------------------------
# compute_auc
# ---------------------------------------------------------------------------
test_that("compute_auc returns 1 for perfect predictions", {
  skip_if_not_installed("pROC")
  y_true <- factor(c(rep("A", 50), rep("B", 50)))
  y_pred <- c(rep(0, 50), rep(1, 50))
  expect_equal(compute_auc(y_true, y_pred), 1.0)
})

test_that("compute_auc returns ~0.5 for random predictions", {
  skip_if_not_installed("pROC")
  set.seed(7)
  y_true <- factor(rep(c("A", "B"), each = 500))
  y_pred <- runif(1000)
  auc <- compute_auc(y_true, y_pred)
  expect_true(abs(auc - 0.5) < 0.1)
})

# ---------------------------------------------------------------------------
# auto_generate_groups
# ---------------------------------------------------------------------------
test_that("auto_generate_groups returns valid group assignments", {
  set.seed(1)
  X <- matrix(rnorm(1000), 50, 20)
  groups <- auto_generate_groups(X)

  expect_length(groups, 20)
  expect_type(groups, "integer")
  expect_true(all(groups >= 1))
  expect_gt(length(unique(groups)), 1)
})

# ---------------------------------------------------------------------------
# fit_regularized_model
# ---------------------------------------------------------------------------
test_that("fit_regularized_model returns expected structure", {
  skip_if_not_installed("glmnet")
  set.seed(1)
  n <- 100; p <- 20
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("G", 1:p)
  y <- factor(rep(c("A", "B"), each = n / 2))
  X[y == "B", 1] <- X[y == "B", 1] + 2
  X[y == "B", 2] <- X[y == "B", 2] - 1.5

  result <- fit_regularized_model(X, y, method = "elastic_net", alpha = 0.5)

  expect_type(result, "list")
  expect_true(all(c("selected", "coefficients", "predict_fn", "lambda",
                    "n_selected", "cv_fit") %in% names(result)))
  expect_true(is.function(result$predict_fn))

  preds <- result$predict_fn(X)
  expect_length(preds, n)
  expect_true(all(preds >= 0 & preds <= 1))
})

test_that("lasso selects a subset of features", {
  skip_if_not_installed("glmnet")
  set.seed(2)
  n <- 100; p <- 50
  X <- matrix(rnorm(n * p), n, p)
  colnames(X) <- paste0("G", 1:p)
  y <- factor(rep(c("A", "B"), each = n / 2))
  X[y == "B", 1:3] <- X[y == "B", 1:3] + 2

  result <- fit_regularized_model(X, y, method = "lasso")
  expect_lt(result$n_selected, p)
})

test_that("unknown regularization method raises error", {
  X <- matrix(1, 10, 2, dimnames = list(NULL, c("A", "B")))
  y <- factor(rep(c("A", "B"), each = 5))
  expect_error(fit_regularized_model(X, y, method = "ridge"), "Unknown regularization")
})
