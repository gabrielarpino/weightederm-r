skip_if_not(weightederm_available, "weightederm Python package not available")

make_huber_data <- function(seed = 5L, n = 80L, p = 3L, true_cp = 40L) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y <- c(
    X[seq_len(true_cp), ]     %*% c(2, -1, 0),
    X[seq(true_cp + 1L, n), ] %*% c(-2, 1, 0)
  ) + rnorm(n, sd = 0.1)
  list(X = X, y = y, true_cp = true_cp, n = n, p = p)
}

## ── Basic fit ────────────────────────────────────────────────────────────────

test_that("werm_huber returns werm_huber / werm_fit", {
  d <- make_huber_data()
  fit <- werm_huber(d$X, d$y, num_chgpts = 1L, delta = 5L,
                   fit_intercept = FALSE)

  expect_s3_class(fit, "werm_fit")
  expect_s3_class(fit, "werm_huber")
})

test_that("werm_huber detects the correct changepoint", {
  d <- make_huber_data()
  fit <- werm_huber(d$X, d$y, num_chgpts = 1L, delta = 5L,
                   fit_intercept = FALSE, max_iter = 200L)

  expect_equal(fit$num_chgpts, 1L)
  expect_length(fit$changepoints, 1L)
  expect_lte(abs(fit$changepoints[1] - d$true_cp), 5L)
})

test_that("changepoints are 1-indexed", {
  d <- make_huber_data()
  fit <- werm_huber(d$X, d$y, num_chgpts = 1L, delta = 5L,
                   fit_intercept = FALSE)

  expect_gte(fit$changepoints[1], 1L)
  expect_lte(fit$changepoints[1], d$n)
})

test_that("fitted attrs have correct shapes", {
  d <- make_huber_data()
  fit <- werm_huber(d$X, d$y, num_chgpts = 1L, delta = 5L,
                   fit_intercept = FALSE)

  expect_equal(fit$num_signals, 2L)
  expect_length(fit$last_segment_coef, d$p)
  expect_null(fit$last_segment_intercept)        # fit_intercept = FALSE
  expect_equal(dim(fit$signal_coefs), c(2L, d$p))
  expect_equal(fit$n_features_in, d$p)
})

test_that("objective is finite", {
  d <- make_huber_data()
  fit <- werm_huber(d$X, d$y, num_chgpts = 1L, delta = 5L,
                   fit_intercept = FALSE)

  expect_true(is.finite(fit$objective))
})

## ── predict ──────────────────────────────────────────────────────────────────

test_that("predict returns numeric vector of correct length", {
  d <- make_huber_data()
  fit <- werm_huber(d$X, d$y, num_chgpts = 1L, delta = 5L,
                   fit_intercept = FALSE)
  X_new <- matrix(rnorm(10L * d$p), nrow = 10L)
  preds <- predict(fit, X_new)

  expect_type(preds, "double")
  expect_length(preds, 10L)
  expect_true(all(is.finite(preds)))
})

## ── num_chgpts = 0 ───────────────────────────────────────────────────────────

test_that("werm_huber works with zero changepoints", {
  set.seed(88L)
  n <- 40L; p <- 2L
  X <- matrix(rnorm(n * p), n, p)
  y <- X %*% c(1, -1) + rnorm(n, sd = 0.1)
  fit <- werm_huber(X, y, num_chgpts = 0L, delta = 3L, fit_intercept = FALSE)

  expect_equal(fit$num_chgpts, 0L)
  expect_length(fit$changepoints, 0L)
})

## ── epsilon parameter ────────────────────────────────────────────────────────

test_that("epsilon parameter is accepted and both fits are valid", {
  d <- make_huber_data()
  fit_small <- werm_huber(d$X, d$y, num_chgpts = 1L, delta = 5L,
                          fit_intercept = FALSE, epsilon = 1.0)
  fit_large <- werm_huber(d$X, d$y, num_chgpts = 1L, delta = 5L,
                          fit_intercept = FALSE, epsilon = 50.0)

  expect_s3_class(fit_small, "werm_fit")
  expect_s3_class(fit_large, "werm_fit")
  expect_true(is.finite(fit_small$objective))
  expect_true(is.finite(fit_large$objective))
})

## ── L2 penalty ───────────────────────────────────────────────────────────────

test_that("L2 penalty shrinks last_segment_coef toward zero", {
  d <- make_huber_data()
  fit_none <- werm_huber(d$X, d$y, num_chgpts = 1L, delta = 5L,
                         fit_intercept = FALSE, penalty = "none")
  fit_l2   <- werm_huber(d$X, d$y, num_chgpts = 1L, delta = 5L,
                         fit_intercept = FALSE, penalty = "l2", alpha = 5.0)

  expect_lte(sqrt(sum(fit_l2$last_segment_coef^2)),
             sqrt(sum(fit_none$last_segment_coef^2)) + 1e-9)
})

## ── robustness to outliers ───────────────────────────────────────────────────

test_that("werm_huber detects changepoint despite large outliers", {
  set.seed(42L)
  n <- 80L; p <- 2L; true_cp <- 40L
  X <- matrix(rnorm(n * p), n, p)
  y <- c(
    X[seq_len(true_cp), ]     %*% c(3, 0),
    X[seq(true_cp + 1L, n), ] %*% c(-3, 0)
  ) + rnorm(n, sd = 0.1)
  # Inject large outliers
  y[c(5L, 15L, 55L, 65L, 75L)] <- y[c(5L, 15L, 55L, 65L, 75L)] + 20

  fit <- werm_huber(X, y, num_chgpts = 1L, delta = 5L,
                   fit_intercept = FALSE, max_iter = 200L)

  expect_lte(abs(fit$changepoints[1] - true_cp), 8L)
})
