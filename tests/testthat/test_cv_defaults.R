skip_if_not(weightederm_available, "weightederm Python package not available")

test_that("automatic folds match explicit folds for every loss and penalty", {
  set.seed(27)
  X <- matrix(rnorm(60), 30, 2)
  regression_y <- rnorm(30)
  wrappers <- list(werm_least_squares_cv, werm_huber_cv, werm_logistic_cv)
  for (i in seq_along(wrappers)) {
    y <- if (i == 3L) rep(c(0, 1, 1, 0, 1), 6) else regression_y
    for (penalty in c("none", "l1", "l2")) {
      for (joint in c(FALSE, TRUE)) {
        args <- list(
          X = X, y = y, max_num_chgpts = 0L, fit_intercept = FALSE,
          penalty = penalty, alpha = 0.1,
          regularization_constants = if (joint) 0.5 else NULL
        )
        automatic <- suppressWarnings(do.call(wrappers[[i]], args))
        args$cv <- if (penalty == "none") 10L else 2L
        explicit <- suppressWarnings(do.call(wrappers[[i]], args))
        expect_equal(automatic$cv_results, explicit$cv_results)
      }
    }
  }
})

test_that("explicit folds override automatic defaults and retain validation", {
  X <- matrix(seq_len(8), ncol = 1)
  y <- rep(c(0, 1, 1, 0), 2)
  for (fun in list(werm_least_squares_cv, werm_huber_cv, werm_logistic_cv)) {
    expect_error(fun(X, y, max_num_chgpts = 0L, penalty = "none"), "cv")
    fit <- fun(X, y, max_num_chgpts = 0L, penalty = "none", cv = 3L)
    expect_true(is.finite(fit$best_score))
    expect_error(fun(X, y, max_num_chgpts = 0L, cv = 1L), "cv")
  }
})
