skip_if_not(weightederm_available, "weightederm Python package not available")

make_logistic_data <- function(seed = 3L, n = 120L, p = 4L, true_cp = 60L) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  eta <- c(
    X[seq_len(true_cp), ]      %*% c(2.5, -2.0, 0, 0),
    X[seq(true_cp + 1L, n), ]  %*% c(-2.5, 2.0, 0, 0)
  )
  y <- rbinom(n, 1L, 1 / (1 + exp(-eta)))
  list(X = X, y = y, true_cp = true_cp, n = n, p = p)
}

## ── Basic fit ────────────────────────────────────────────────────────────────

test_that("werm_logistic returns werm_logistic / werm_fit", {
  d <- make_logistic_data()
  fit <- werm_logistic(d$X, d$y, num_chgpts = 1L, delta = 5L,
                       fit_intercept = FALSE)

  expect_s3_class(fit, "werm_fit")
  expect_s3_class(fit, "werm_logistic")
})

test_that("werm_logistic detects correct changepoint", {
  d <- make_logistic_data()
  fit <- werm_logistic(d$X, d$y, num_chgpts = 1L, delta = 5L,
                       fit_intercept = FALSE, max_iter = 300L)

  expect_lte(abs(fit$changepoints[1] - d$true_cp), 6L)
})

test_that("classes attribute has length 2", {
  d <- make_logistic_data()
  fit <- werm_logistic(d$X, d$y, num_chgpts = 1L, delta = 5L,
                       fit_intercept = FALSE)

  expect_length(fit$classes, 2L)
  expect_type(fit$classes, "character")
})

## ── predict ──────────────────────────────────────────────────────────────────

test_that("predict(type='class') returns character labels", {
  d <- make_logistic_data()
  fit <- werm_logistic(d$X, d$y, num_chgpts = 1L, delta = 5L,
                       fit_intercept = FALSE)
  X_new <- matrix(rnorm(20L * d$p), 20L, d$p)
  preds <- predict(fit, X_new, type = "class")

  expect_type(preds, "character")
  expect_length(preds, 20L)
  expect_true(all(preds %in% fit$classes))
})

test_that("predict(type='prob') returns matrix with two columns summing to 1", {
  d <- make_logistic_data()
  fit <- werm_logistic(d$X, d$y, num_chgpts = 1L, delta = 5L,
                       fit_intercept = FALSE)
  X_new <- matrix(rnorm(15L * d$p), 15L, d$p)
  probs <- predict(fit, X_new, type = "prob")

  expect_equal(ncol(probs), 2L)
  expect_equal(nrow(probs), 15L)
  expect_true(all(abs(rowSums(probs) - 1) < 1e-9))
  expect_true(all(probs >= 0 & probs <= 1))
})
