skip_if_not(weightederm_available, "weightederm Python package not available")

## ── Shared data ─────────────────────────────────────────────────────────────

make_ls_data <- function(seed = 1L, n = 80L, p = 4L, true_cp = 40L) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  beta_left  <- c(2, -1, 0, 0)
  beta_right <- c(-1, 2, 0, 0)
  y <- c(
    X[seq_len(true_cp), ] %*% beta_left  + rnorm(true_cp,  sd = 0.1),
    X[seq(true_cp + 1L, n), ] %*% beta_right + rnorm(n - true_cp, sd = 0.1)
  )
  list(X = X, y = y, true_cp = true_cp, n = n, p = p)
}

## ── Basic fit ────────────────────────────────────────────────────────────────

test_that("werm_least_squares returns werm_fit with correct classes", {
  d <- make_ls_data()
  fit <- werm_least_squares(d$X, d$y, num_chgpts = 1L, delta = 5L,
                             fit_intercept = FALSE)

  expect_s3_class(fit, "werm_fit")
  expect_s3_class(fit, "werm_least_squares")
})

test_that("werm_least_squares detects the correct changepoint", {
  d <- make_ls_data()
  fit <- werm_least_squares(d$X, d$y, num_chgpts = 1L, delta = 5L,
                             fit_intercept = FALSE)

  expect_equal(fit$num_chgpts, 1L)
  expect_length(fit$changepoints, 1L)
  expect_lte(abs(fit$changepoints[1] - d$true_cp), 5L)
})

test_that("changepoints are 1-indexed", {
  d <- make_ls_data()
  fit <- werm_least_squares(d$X, d$y, num_chgpts = 1L, delta = 5L,
                             fit_intercept = FALSE)

  expect_gte(fit$changepoints[1], 1L)
  expect_lte(fit$changepoints[1], d$n)
})

test_that("fitted attrs have correct shapes", {
  d <- make_ls_data()
  fit <- werm_least_squares(d$X, d$y, num_chgpts = 1L, delta = 5L,
                             fit_intercept = FALSE)

  expect_equal(fit$num_signals, 2L)
  expect_length(fit$last_segment_coef, d$p)
  expect_null(fit$last_segment_intercept)        # fit_intercept = FALSE
  expect_equal(dim(fit$signal_coefs), c(2L, d$p))
  expect_equal(fit$n_features_in, d$p)
})

test_that("objective is finite", {
  d <- make_ls_data()
  fit <- werm_least_squares(d$X, d$y, num_chgpts = 1L, delta = 5L,
                             fit_intercept = FALSE)

  expect_true(is.finite(fit$objective))
})

## ── predict ──────────────────────────────────────────────────────────────────

test_that("predict returns numeric vector of correct length", {
  d <- make_ls_data()
  fit <- werm_least_squares(d$X, d$y, num_chgpts = 1L, delta = 5L,
                             fit_intercept = FALSE)
  X_new <- matrix(rnorm(10L * d$p), nrow = 10L)
  preds <- predict(fit, X_new)

  expect_type(preds, "double")
  expect_length(preds, 10L)
  expect_true(all(is.finite(preds)))
})

## ── coef ─────────────────────────────────────────────────────────────────────

test_that("coef() returns last_segment_coef", {
  d <- make_ls_data()
  fit <- werm_least_squares(d$X, d$y, num_chgpts = 1L, delta = 5L,
                             fit_intercept = FALSE)

  expect_equal(coef(fit), fit$last_segment_coef)
})

## ── num_chgpts = 0 ───────────────────────────────────────────────────────────

test_that("werm_least_squares works with zero changepoints", {
  set.seed(99)
  n <- 40L; p <- 2L
  X <- matrix(rnorm(n * p), n, p)
  y <- X %*% c(1, -1) + rnorm(n, sd = 0.1)
  fit <- werm_least_squares(X, y, num_chgpts = 0L, delta = 3L,
                             fit_intercept = FALSE)

  expect_equal(fit$num_chgpts, 0L)
  expect_length(fit$changepoints, 0L)
})

## ── L2 penalty ───────────────────────────────────────────────────────────────

test_that("L2 penalty shrinks last_segment_coef toward zero", {
  d <- make_ls_data()
  fit_none <- werm_least_squares(d$X, d$y, num_chgpts = 1L, delta = 5L,
                                  fit_intercept = FALSE, penalty = "none")
  fit_l2   <- werm_least_squares(d$X, d$y, num_chgpts = 1L, delta = 5L,
                                  fit_intercept = FALSE, penalty = "l2",
                                  alpha = 5.0)

  expect_lte(sqrt(sum(fit_l2$last_segment_coef^2)),
             sqrt(sum(fit_none$last_segment_coef^2)) + 1e-9)
})

## ── print / summary smoke test ───────────────────────────────────────────────

test_that("print.werm_fit runs without error", {
  d <- make_ls_data()
  fit <- werm_least_squares(d$X, d$y, num_chgpts = 1L, delta = 5L,
                             fit_intercept = FALSE)
  expect_output(print(fit), "WERM Changepoint Estimator")
  expect_output(print(fit), "1-indexed")
})
