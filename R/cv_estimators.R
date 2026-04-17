# ---------------------------------------------------------------------------
# CV estimators
# ---------------------------------------------------------------------------

#' Fit a WERM changepoint model with squared loss and CV selection
#'
#' Selects the number of changepoints from `{0, …, max_num_chgpts}` by
#' interleaved cross-validation, then refits on the full data.
#'
#' @param X Numeric matrix of shape `(n, p)`. Observations must be in order.
#' @param y Numeric vector of length `n`. Ordered responses.
#' @param max_num_chgpts Integer. Upper bound of the CV search grid.
#' @param delta Integer. Minimum gap between candidate changepoints.
#'   Default `1`.
#' @param search_method Character. `"efficient"` (default) or `"brute_force"`.
#' @param cv Integer. Number of interleaved folds. Default `5`.
#' @param fit_intercept Logical. Default `TRUE`.
#' @param use_base_loss_for_cv Logical. If `TRUE`, uses squared loss for CV
#'   segment fits and scoring instead of the default absolute error.
#'   Default `FALSE`.
#' @param penalty Character. Passed to the inner fixed model. Default `"none"`.
#' @param alpha Numeric. Penalty strength. Default `0`.
#'
#' @return An object of class `c("werm_least_squares_cv", "werm_fit")` with all
#'   elements from [werm_least_squares()] plus:
#'   \describe{
#'     \item{`best_num_chgpts`}{Integer. CV-selected number of changepoints.}
#'     \item{`best_score`}{Numeric. Mean held-out score for the best model.}
#'     \item{`cv_results`}{Data frame with columns `num_chgpts` and
#'       `mean_test_score`.}
#'     \item{`num_chgpts_grid`}{Integer vector. Full CV search grid.}
#'     \item{`segment_bounds`}{List of integer pairs `[start, stop]`
#'       (1-indexed, half-open).}
#'     \item{`segment_coefs`}{Matrix `(num_signals x p)`.}
#'     \item{`segment_intercepts`}{Numeric vector or `NULL`.}
#'   }
#'
#' @examples
#' if (reticulate::py_module_available("weightederm")) {
#'   set.seed(10)
#'   n <- 45L; p <- 2L
#'   X <- matrix(rnorm(n * p), n, p)
#'   y <- c(
#'     X[1:15, ] %*% c(3, 0),
#'     X[16:30, ] %*% c(-3, 0),
#'     X[31:45, ] %*% c(3, 0)
#'   ) + rnorm(n, sd = 0.1)
#'   fit <- werm_least_squares_cv(X, y, max_num_chgpts = 2L, cv = 3L,
#'                                delta = 4L, fit_intercept = FALSE)
#'   fit$best_num_chgpts
#'   fit$changepoints
#'   fit$cv_results
#' }
#'
#' @export
werm_least_squares_cv <- function(
    X, y,
    max_num_chgpts,
    delta                = 1L,
    search_method        = "efficient",
    cv                   = 5L,
    fit_intercept        = TRUE,
    use_base_loss_for_cv = FALSE,
    penalty              = "none",
    alpha                = 0.0
) {
  py <- .weightederm_py()
  X_py <- .to_numpy_matrix(X)
  y_py <- .to_numpy_vector(y)

  py_model <- py$WERMLeastSquaresCV(
    max_num_chgpts       = as.integer(max_num_chgpts),
    delta                = as.integer(delta),
    search_method        = search_method,
    cv                   = as.integer(cv),
    fit_intercept        = fit_intercept,
    use_base_loss_for_cv = use_base_loss_for_cv,
    penalty              = penalty,
    alpha                = as.numeric(alpha)
  )
  py_model$fit(X_py, y_py)

  attrs <- .extract_cv_attrs(py_model)
  .build_werm_object(attrs, "least_squares_cv", py_model)
}


#' Fit a WERM changepoint model with Huber loss and CV selection
#'
#' @inheritParams werm_least_squares_cv
#' @param epsilon Numeric. Huber transition parameter. Default `1.35`.
#' @param max_iter Integer. Maximum L-BFGS-B iterations. Default `100`.
#' @param tol Numeric. Gradient-norm tolerance. Default `1e-5`.
#'
#' @return An object of class `c("werm_huber_cv", "werm_fit")`.
#'
#' @examples
#' if (reticulate::py_module_available("weightederm")) {
#'   set.seed(11)
#'   n <- 40L; p <- 2L
#'   X <- matrix(rnorm(n * p), n, p)
#'   y <- c(
#'     X[1:20, ] %*% c(2, 0),
#'     X[21:40, ] %*% c(-2, 0)
#'   ) + rnorm(n, sd = 0.1)
#'   fit <- werm_huber_cv(X, y, max_num_chgpts = 2L, cv = 3L, delta = 4L,
#'                        fit_intercept = FALSE)
#'   fit$best_num_chgpts
#' }
#'
#' @export
werm_huber_cv <- function(
    X, y,
    max_num_chgpts,
    delta                = 1L,
    search_method        = "efficient",
    cv                   = 5L,
    fit_intercept        = TRUE,
    epsilon              = 1.35,
    max_iter             = 100L,
    tol                  = 1e-5,
    use_base_loss_for_cv = FALSE,
    penalty              = "none",
    alpha                = 0.0
) {
  py <- .weightederm_py()
  X_py <- .to_numpy_matrix(X)
  y_py <- .to_numpy_vector(y)

  py_model <- py$WERMHuberCV(
    max_num_chgpts       = as.integer(max_num_chgpts),
    delta                = as.integer(delta),
    search_method        = search_method,
    cv                   = as.integer(cv),
    fit_intercept        = fit_intercept,
    epsilon              = as.numeric(epsilon),
    max_iter             = as.integer(max_iter),
    tol                  = as.numeric(tol),
    use_base_loss_for_cv = use_base_loss_for_cv,
    penalty              = penalty,
    alpha                = as.numeric(alpha)
  )
  py_model$fit(X_py, y_py)

  attrs <- .extract_cv_attrs(py_model)
  .build_werm_object(attrs, "huber_cv", py_model)
}


#' Fit a WERM changepoint model with logistic loss and CV selection
#'
#' @inheritParams werm_least_squares_cv
#' @param y Integer or factor vector of binary labels.
#' @param max_iter Integer. Maximum L-BFGS-B iterations. Default `100`.
#' @param tol Numeric. Gradient-norm tolerance. Default `1e-5`.
#' @param use_base_loss_for_cv Logical. If `TRUE`, uses logistic loss for CV
#'   segment scoring instead of the default absolute error. Default `FALSE`.
#' @param penalty Character. Default `"l2"`.
#' @param alpha Numeric. Default `1.0`.
#'
#' @return An object of class `c("werm_logistic_cv", "werm_fit")`.
#'   Contains all CV elements plus `classes` (character vector of length 2).
#'
#' @examples
#' if (reticulate::py_module_available("weightederm")) {
#'   set.seed(12)
#'   n <- 60L; p <- 2L
#'   X <- matrix(rnorm(n * p), n, p)
#'   eta <- c(
#'     X[1:30, ] %*% c(2, -2),
#'     X[31:60, ] %*% c(-2, 2)
#'   )
#'   y <- rbinom(n, 1L, 1 / (1 + exp(-eta)))
#'   fit <- werm_logistic_cv(X, y, max_num_chgpts = 2L, cv = 3L, delta = 4L,
#'                           fit_intercept = FALSE, max_iter = 200L)
#'   fit$best_num_chgpts
#'   fit$changepoints
#' }
#'
#' @export
werm_logistic_cv <- function(
    X, y,
    max_num_chgpts,
    delta                = 1L,
    search_method        = "efficient",
    cv                   = 5L,
    fit_intercept        = TRUE,
    max_iter             = 100L,
    tol                  = 1e-5,
    use_base_loss_for_cv = FALSE,
    penalty              = "l2",
    alpha                = 1.0
) {
  py <- .weightederm_py()
  X_py <- .to_numpy_matrix(X)
  y_py <- .to_numpy_vector(y)

  py_model <- py$WERMLogisticCV(
    max_num_chgpts       = as.integer(max_num_chgpts),
    delta                = as.integer(delta),
    search_method        = search_method,
    cv                   = as.integer(cv),
    fit_intercept        = fit_intercept,
    max_iter             = as.integer(max_iter),
    tol                  = as.numeric(tol),
    use_base_loss_for_cv = use_base_loss_for_cv,
    penalty              = penalty,
    alpha                = as.numeric(alpha)
  )
  py_model$fit(X_py, y_py)

  attrs <- .extract_cv_attrs(py_model)
  attrs$classes <- as.character(reticulate::py_to_r(py_model$classes_))

  .build_werm_object(attrs, "logistic_cv", py_model)
}
