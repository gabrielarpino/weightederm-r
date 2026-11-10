test_that("all wrappers default to exact DP", {
  for (fun in list(werm_least_squares, werm_huber, werm_logistic,
                   werm_least_squares_cv, werm_huber_cv, werm_logistic_cv)) {
    expect_identical(formals(fun)$search_method, "dp")
  }
})

test_that("custom marginals and joint CV reach Python", {
  skip_if_not(weightederm_available)
  X <- matrix(seq_len(24), ncol = 2)
  y <- as.numeric(seq_len(12))
  fit <- werm_least_squares(X, y, num_chgpts = 0L,
                            marginal_weights = matrix(1, 1, 12))
  expect_length(fit$changepoints, 0)
  cv <- werm_least_squares_cv(X, y, max_num_chgpts = 0L, cv = 2L,
                              penalty = "l2", regularization_constants = c(.1, 1))
  expect_length(cv$cv_results$regularization_constant, 2)
})
