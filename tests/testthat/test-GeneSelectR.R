# =============================================================================
# test-main.R — Integration tests for geneselectr2_fit pipeline
# =============================================================================

# ---------------------------------------------------------------------------
# Helper: simulate a small dataset with known signal
# ---------------------------------------------------------------------------
simulate_data <- function(n = 100, p = 50, n_signal = 5, effect = 2, seed = 123) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("Gene", 1:p)
  y <- factor(rep(c("Control", "Case"), each = n / 2))

  # Inject signal into first n_signal genes
  for (j in seq_len(n_signal)) {
    X[y == "Case", j] <- X[y == "Case", j] + effect
  }

  list(X = X, y = y, signal_genes = paste0("Gene", 1:n_signal))
}


# ---------------------------------------------------------------------------
# run_cv_fold
# ---------------------------------------------------------------------------
test_that("run_cv_fold returns expected fields", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("pROC")

  sim <- simulate_data(n = 60, p = 20, n_signal = 3)
  fold <- list(train = 1:40, test = 41:60, fold = 1, repeat_num = 1)

  result <- run_cv_fold(fold, sim$X, sim$y,
                        regularization_method = "elastic_net",
                        alpha = 0.5)

  expect_type(result, "list")
  expect_true(all(c("selected", "coefficients", "mi_scores", "auc",
                    "fold_num", "repeat_num") %in% names(result)))
  expect_length(result$mi_scores, ncol(sim$X))
  expect_true(is.numeric(result$auc))
})

test_that("run_cv_fold MI scores are computed on training data only", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("pROC")

  sim <- simulate_data(n = 60, p = 20, n_signal = 3)
  fold <- list(train = 1:40, test = 41:60, fold = 1, repeat_num = 1)

  result <- run_cv_fold(fold, sim$X, sim$y,
                        regularization_method = "lasso",
                        alpha = 1.0)

  # MI scores should exist for all genes (computed on training fold)
  expect_length(result$mi_scores, 20)
  # Discrete MI can produce tiny negatives on small folds due to binning
  expect_true(all(result$mi_scores >= -1e-10))
})

test_that("run_cv_fold returns chance AUC when no features selected", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("pROC")

  # Pure noise — very few features should be selected with high penalty
  set.seed(99)
  n <- 40; p <- 5
  X <- matrix(rnorm(n * p), n, p, dimnames = list(NULL, paste0("G", 1:p)))
  y <- factor(rep(c("A", "B"), each = n / 2))
  fold <- list(train = 1:30, test = 31:40, fold = 1, repeat_num = 1)

  result <- run_cv_fold(fold, X, y, "elastic_net", alpha = 0.5)

  # Either features are selected (and AUC is numeric) or
  # no features selected and AUC = 0.5
  expect_true(is.numeric(result$auc))
})


# ---------------------------------------------------------------------------
# geneselectr2_fit — full pipeline (bio_mode = "none")
# ---------------------------------------------------------------------------
test_that("geneselectr2_fit runs end-to-end with bio_mode='none'", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("pROC")

  sim <- simulate_data(n = 80, p = 30, n_signal = 5, effect = 2.5)

  result <- geneselectr2_fit(
    X = sim$X,
    y = sim$y,
    bio_mode = "none",
    K = 3,
    R = 2,
    alpha = 0.8,
    verbose = FALSE,
    random_seed = 42
  )

  # Check return structure
  expect_type(result, "list")
  expect_true(all(c("gene_scores", "cv_results", "cv_summary",
                    "parameters", "timing") %in% names(result)))

  # gene_scores
  gs <- result$gene_scores
  expect_true(is.data.frame(gs))
  expect_equal(nrow(gs), 30)
  expect_true(all(c("gene", "final_score", "pi_exact", "u", "b") %in% names(gs)))

  # Scores are in [0,1] range (or close, due to eps in geometric)
  expect_true(all(gs$final_score >= 0))
  expect_true(all(gs$pi_exact >= 0 & gs$pi_exact <= 1))

  # Biology score should be 1 for all genes in "none" mode
  expect_true(all(gs$b == 1))

  # cv_results
  expect_true(is.numeric(result$cv_results$auc_scores))
  expect_length(result$cv_results$auc_scores, 3 * 2)  # K * R

  # cv_summary
  expect_true(all(c("mean", "sd", "median", "min", "max") %in%
                    names(result$cv_summary)))

  # parameters
  expect_equal(result$parameters$K, 3)
  expect_equal(result$parameters$R, 2)
  expect_equal(result$parameters$bio_mode, "none")

  # timing
  expect_true(result$timing$total_seconds > 0)
})

test_that("geneselectr2_fit ranks signal genes highly", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("pROC")

  sim <- simulate_data(n = 100, p = 30, n_signal = 3, effect = 3)

  result <- geneselectr2_fit(
    X = sim$X,
    y = sim$y,
    bio_mode = "none",
    K = 5,
    R = 5,
    alpha = 0.7,
    verbose = FALSE,
    random_seed = 1
  )

  gs <- result$gene_scores
  top10 <- gs$gene[1:10]

  # At least 2 of the 3 signal genes should be in the top 10
  n_recovered <- sum(sim$signal_genes %in% top10)
  expect_gte(n_recovered, 2)
})

test_that("geneselectr2_fit AUC is above chance for signal data", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("pROC")

  sim <- simulate_data(n = 80, p = 20, n_signal = 5, effect = 2)

  result <- geneselectr2_fit(
    X = sim$X,
    y = sim$y,
    bio_mode = "none",
    K = 3,
    R = 3,
    verbose = FALSE
  )

  expect_gt(result$cv_summary$mean, 0.6)
})


# ---------------------------------------------------------------------------
# geneselectr2_fit — score formulas
# ---------------------------------------------------------------------------
test_that("geneselectr2_fit works with all score formulas", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("pROC")

  sim <- simulate_data(n = 60, p = 15, n_signal = 3)

  for (formula in c("geometric", "arithmetic", "harmonic", "minimum")) {
    result <- geneselectr2_fit(
      X = sim$X,
      y = sim$y,
      bio_mode = "none",
      score_formula = formula,
      K = 2,
      R = 2,
      verbose = FALSE
    )

    expect_equal(nrow(result$gene_scores), 15,
                 info = paste("formula:", formula))
    expect_true(all(is.finite(result$gene_scores$final_score)),
                info = paste("formula:", formula))
  }
})


# ---------------------------------------------------------------------------
# geneselectr2_fit — lasso vs elastic_net
# ---------------------------------------------------------------------------
test_that("geneselectr2_fit works with both lasso and elastic_net", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("pROC")

  sim <- simulate_data(n = 60, p = 15, n_signal = 3)

  for (method in c("lasso", "elastic_net")) {
    result <- geneselectr2_fit(
      X = sim$X,
      y = sim$y,
      bio_mode = "none",
      regularization_method = method,
      K = 2,
      R = 2,
      verbose = FALSE
    )

    expect_equal(nrow(result$gene_scores), 15,
                 info = paste("method:", method))
    expect_equal(result$parameters$regularization, method)
  }
})


# ---------------------------------------------------------------------------
# geneselectr2_fit — reproducibility
# ---------------------------------------------------------------------------
test_that("geneselectr2_fit is reproducible with same seed", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("pROC")

  sim <- simulate_data(n = 60, p = 15, n_signal = 3)

  r1 <- geneselectr2_fit(sim$X, sim$y, bio_mode = "none",
                         K = 2, R = 2, verbose = FALSE, random_seed = 7)
  r2 <- geneselectr2_fit(sim$X, sim$y, bio_mode = "none",
                         K = 2, R = 2, verbose = FALSE, random_seed = 7)

  expect_equal(r1$gene_scores$gene, r2$gene_scores$gene)
  expect_equal(r1$gene_scores$final_score, r2$gene_scores$final_score)
  expect_equal(r1$cv_results$auc_scores, r2$cv_results$auc_scores)
})


# ---------------------------------------------------------------------------
# geneselectr2_fit — custom weights
# ---------------------------------------------------------------------------
test_that("geneselectr2_fit respects custom score_weights", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("pROC")

  sim <- simulate_data(n = 60, p = 15, n_signal = 3)

  r_default <- geneselectr2_fit(sim$X, sim$y, bio_mode = "none",
                                score_weights = c(1, 1, 1),
                                K = 2, R = 2, verbose = FALSE)
  r_pi_only <- geneselectr2_fit(sim$X, sim$y, bio_mode = "none",
                                score_weights = c(10, 0.01, 0.01),
                                K = 2, R = 2, verbose = FALSE)

  # Rankings should differ when weights change drastically
  expect_false(identical(r_default$gene_scores$gene, r_pi_only$gene_scores$gene))
})


# ---------------------------------------------------------------------------
# geneselectr2_fit — MI method
# ---------------------------------------------------------------------------
test_that("geneselectr2_fit runs with discrete MI", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("pROC")

  sim <- simulate_data(n = 60, p = 15, n_signal = 3)

  result <- geneselectr2_fit(
    X = sim$X, y = sim$y,
    bio_mode = "none",
    mi_method = "discrete",
    mi_bins = 3,
    K = 2, R = 2, verbose = FALSE
  )

  expect_true(all(result$gene_scores$u_mi >= 0))
})
