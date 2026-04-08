skip_if_not(weightederm_available, "weightederm Python package not available")

## R translations of the M1, M2, M3 minimal working examples from the Python
## README. Data generation uses the same structure and signal strength as the
## Python examples but with R's RNG (different seeds → different draws, same
## detection quality).

## ── M1-style: fixed num_chgpts, sparse linear regression ─────────────────────

test_that("M1: werm_least_squares detects changepoint near position 60", {
  set.seed(0L)
  n <- 120L; p <- 20L; true_cp <- 60L
  X <- matrix(rnorm(n * p), n, p)

  beta_left  <- rep(0, p); beta_left[c(1L, 4L)]  <- c(2.0, -1.5)
  beta_right <- rep(0, p); beta_right[c(1L, 4L)] <- c(-1.0, 2.5)

  y <- numeric(n)
  y[seq_len(true_cp)]       <- X[seq_len(true_cp), ]       %*% beta_left  +
    rnorm(true_cp,      sd = 0.2)
  y[seq(true_cp + 1L, n)] <- X[seq(true_cp + 1L, n), ] %*% beta_right +
    rnorm(n - true_cp,  sd = 0.2)

  fit <- werm_least_squares(X, y, num_chgpts = 1L, delta = 5L,
                             search_method = "efficient",
                             fit_intercept = FALSE)

  expect_equal(fit$num_chgpts, 1L)
  expect_lte(abs(fit$changepoints[1] - true_cp), 5L)
})

## ── M2-style: unknown num_chgpts, CV selection ────────────────────────────────

test_that("M2: werm_least_squares_cv selects 2 changepoints near positions 60 and 120", {
  set.seed(1L)
  n <- 180L; p <- 10L
  true_cps <- c(60L, 120L)
  X <- matrix(rnorm(n * p), n, p)

  beta_1 <- rep(0, p); beta_1[1L]        <- 3.5
  beta_2 <- rep(0, p); beta_2[c(1L, 2L)] <- c(-3.0, 3.0)
  beta_3 <- rep(0, p); beta_3[1L:3L]     <- c(2.5, -2.5, 2.5)

  y <- numeric(n)
  y[1L:60L]    <- X[1L:60L,    ] %*% beta_1 + rnorm(60L, sd = 0.05)
  y[61L:120L]  <- X[61L:120L,  ] %*% beta_2 + rnorm(60L, sd = 0.05)
  y[121L:180L] <- X[121L:180L, ] %*% beta_3 + rnorm(60L, sd = 0.05)

  fit <- werm_least_squares_cv(X, y, max_num_chgpts = 2L, delta = 5L, cv = 3L,
                                search_method = "efficient",
                                fit_intercept = FALSE)

  expect_equal(fit$best_num_chgpts, 2L)
  expect_lte(max(abs(sort(fit$changepoints) - true_cps)), 5L)
})

## ── M3-style: fixed num_chgpts, binary logistic regression ───────────────────

test_that("M3: werm_logistic detects changepoint near position 80", {
  set.seed(2L)
  n <- 160L; p <- 12L; true_cp <- 80L
  X <- matrix(rnorm(n * p), n, p)

  beta_left  <- rep(0, p); beta_left[c(1L, 3L)]  <- c(2.5, -2.0)
  beta_right <- rep(0, p); beta_right[c(1L, 3L)] <- c(-2.5, 2.0)

  eta <- numeric(n)
  eta[seq_len(true_cp)]       <- X[seq_len(true_cp), ]       %*% beta_left
  eta[seq(true_cp + 1L, n)] <- X[seq(true_cp + 1L, n), ] %*% beta_right
  prob <- 1.0 / (1.0 + exp(-eta))
  y <- rbinom(n, 1L, prob)

  fit <- werm_logistic(X, y, num_chgpts = 1L, delta = 5L,
                       search_method = "efficient", fit_intercept = FALSE,
                       max_iter = 300L, tol = 1e-6)

  expect_s3_class(fit, "werm_fit")
  expect_equal(fit$num_chgpts, 1L)
  expect_lte(abs(fit$changepoints[1] - true_cp), 5L)
})
