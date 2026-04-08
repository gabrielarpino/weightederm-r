skip_if_not(weightederm_available, "weightederm Python package not available")

make_two_cp_data <- function(seed = 10L, n = 90L, p = 2L) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), n, p)
  y <- c(
    X[1:30,  ] %*% c(3, 0)  + rnorm(30,  sd = 0.05),
    X[31:60, ] %*% c(-3, 0) + rnorm(30,  sd = 0.05),
    X[61:90, ] %*% c(3, 0)  + rnorm(30,  sd = 0.05)
  )
  list(X = X, y = y, true_cps = c(30L, 60L), n = n, p = p)
}

## ── WERMLeastSquaresCV ───────────────────────────────────────────────────────

test_that("werm_least_squares_cv selects correct num_chgpts", {
  d <- make_two_cp_data()
  fit <- werm_least_squares_cv(d$X, d$y, max_num_chgpts = 4L, cv = 3L,
                                delta = 5L, fit_intercept = FALSE)

  expect_equal(fit$best_num_chgpts, 2L)
})

test_that("cv_results has correct structure", {
  d <- make_two_cp_data()
  fit <- werm_least_squares_cv(d$X, d$y, max_num_chgpts = 3L, cv = 3L,
                                delta = 5L, fit_intercept = FALSE)

  expect_s3_class(fit$cv_results, "data.frame")
  expect_named(fit$cv_results, c("num_chgpts", "mean_test_score"))
  expect_equal(nrow(fit$cv_results), 4L)  # 0:3 inclusive
})

test_that("segment_bounds are 1-indexed and cover the full range", {
  d <- make_two_cp_data()
  fit <- werm_least_squares_cv(d$X, d$y, max_num_chgpts = 3L, cv = 3L,
                                delta = 5L, fit_intercept = FALSE)

  bounds <- fit$segment_bounds
  expect_equal(bounds[[1]][[1]], 1L)              # first segment starts at 1
  expect_equal(bounds[[length(bounds)]][[2]], d$n) # last segment ends at n
})

test_that("changepoints are close to true values", {
  d <- make_two_cp_data()
  fit <- werm_least_squares_cv(d$X, d$y, max_num_chgpts = 4L, cv = 3L,
                                delta = 5L, fit_intercept = FALSE)

  expect_equal(fit$num_chgpts, 2L)
  expect_lte(abs(fit$changepoints[1] - d$true_cps[1]), 5L)
  expect_lte(abs(fit$changepoints[2] - d$true_cps[2]), 5L)
})

test_that("predict works on CV estimator", {
  d <- make_two_cp_data()
  fit <- werm_least_squares_cv(d$X, d$y, max_num_chgpts = 3L, cv = 3L,
                                delta = 5L, fit_intercept = FALSE)
  X_new <- matrix(rnorm(8L * d$p), 8L, d$p)
  preds <- predict(fit, X_new)

  expect_length(preds, 8L)
  expect_true(all(is.finite(preds)))
})

## ── WERMHuberCV smoke test ───────────────────────────────────────────────────

test_that("werm_huber_cv runs and returns werm_fit", {
  d <- make_two_cp_data()
  fit <- werm_huber_cv(d$X, d$y, max_num_chgpts = 3L, cv = 3L,
                       delta = 5L, fit_intercept = FALSE)

  expect_s3_class(fit, "werm_fit")
  expect_true(fit$best_num_chgpts >= 0L)
})

## ── WERMLogisticCV smoke test ────────────────────────────────────────────────

test_that("werm_logistic_cv runs and returns werm_fit with classes", {
  set.seed(20L)
  n <- 80L; p <- 3L; true_cp <- 40L
  X  <- matrix(rnorm(n * p), n, p)
  eta <- c(X[1:true_cp,     ] %*% c(2, -1, 0),
           X[(true_cp+1):n, ] %*% c(-2, 1, 0))
  y  <- rbinom(n, 1L, 1 / (1 + exp(-eta)))

  fit <- werm_logistic_cv(X, y, max_num_chgpts = 2L, cv = 3L,
                           delta = 5L, fit_intercept = FALSE)

  expect_s3_class(fit, "werm_fit")
  expect_length(fit$classes, 2L)
  expect_true(fit$best_num_chgpts >= 0L)
})
