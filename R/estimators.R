# ---------------------------------------------------------------------------
# Fixed-changepoint estimators
# ---------------------------------------------------------------------------

#' Fit a WERM changepoint model with squared loss
#'
#' Detects `num_chgpts` changepoints in ordered regression data by minimising
#' a Weighted Empirical Risk with squared loss.
#'
#' @param X Numeric matrix of shape `(n, p)`. Observations must be in order.
#' @param y Numeric vector of length `n`. Ordered responses.
#' @param num_chgpts Integer. Number of changepoints to detect.
#' @param delta Integer. Minimum gap between candidate changepoints during
#'   search. Default `1`. Rule of thumb: `max(1L, as.integer(0.05 * nrow(X)))`.
#' @param search_method Character. `"efficient"` (default) or `"brute_force"`.
#' @param fit_intercept Logical. Whether each segment model includes an
#'   intercept. Default `TRUE`.
#' @param fit_solver Character. `"direct"` (default, closed-form) or
#'   `"lbfgsb"` (gradient-based).
#' @param penalty Character. `"none"` (default), `"l1"`, or `"l2"`.
#' @param alpha Numeric. Penalty strength. Default `0`.
#'
#' @return An object of class `c("werm_least_squares", "werm_fit")` with the
#'   following named elements:
#'   \describe{
#'     \item{`changepoints`}{Integer vector of detected changepoints
#'       (**1-indexed**, R convention). Length equals `num_chgpts`.}
#'     \item{`num_chgpts`}{Integer. Number of detected changepoints.}
#'     \item{`num_signals`}{Integer. Number of segments (`num_chgpts + 1`).}
#'     \item{`objective`}{Numeric. Minimised WERM objective value.}
#'     \item{`last_segment_coef`}{Numeric vector. Coefficient of the
#'       unweighted base-loss refit on the last segment (used by `predict()`).}
#'     \item{`last_segment_intercept`}{Numeric or `NULL`.}
#'     \item{`signal_coefs`}{Matrix `(num_signals x p)`. Stage-1 WERM
#'       coefficient estimates.}
#'     \item{`signal_intercepts`}{Numeric vector or `NULL`.}
#'     \item{`n_features_in`}{Integer. Number of features.}
#'   }
#'
#' @examples
#' # Limit BLAS/OpenMP threads so example CPU time stays proportional to
#' # elapsed time on multicore CRAN machines.
#' Sys.setenv(
#'   OMP_NUM_THREADS = "1",
#'   OPENBLAS_NUM_THREADS = "1",
#'   MKL_NUM_THREADS = "1",
#'   BLAS_NUM_THREADS = "1"
#' )
#'
#' if (nzchar(Sys.getenv("RETICULATE_PYTHON")) &&
#'     weightederm:::.weightederm_examples_available("WERMLeastSquares")) {
#'   set.seed(1)
#'   n <- 24L; p <- 2L; true_cp <- 12L
#'   X <- matrix(rnorm(n * p), n, p)
#'   y <- c(
#'     X[1:true_cp, ] %*% c(3, -1.5),
#'     X[(true_cp + 1L):n, ] %*% c(-3, 1.5)
#'   ) + rnorm(n, sd = 0.05)
#'   fit <- werm_least_squares(X, y, num_chgpts = 1L, delta = 3L,
#'                             fit_intercept = FALSE)
#'   fit$changepoints
#' }
#'
#' @export
werm_least_squares <- function(
    X, y,
    num_chgpts,
    delta          = 1L,
    search_method  = "efficient",
    fit_intercept  = TRUE,
    fit_solver     = "direct",
    penalty        = "none",
    alpha          = 0.0
) {
  py <- .weightederm_py()
  X_py <- .to_numpy_matrix(X)
  y_py <- .to_numpy_vector(y)

  py_model <- py$WERMLeastSquares(
    num_chgpts    = as.integer(num_chgpts),
    delta         = as.integer(delta),
    search_method = search_method,
    fit_intercept = fit_intercept,
    fit_solver    = fit_solver,
    penalty       = penalty,
    alpha         = as.numeric(alpha)
  )
  py_model$fit(X_py, y_py)

  attrs <- .extract_fixed_attrs(py_model)
  .build_werm_object(attrs, "least_squares", py_model)
}


#' Fit a WERM changepoint model with Huber loss
#'
#' Like [werm_least_squares()] but uses the Huber loss, which is more robust
#' to outliers than squared loss.
#'
#' @inheritParams werm_least_squares
#' @param epsilon Numeric. Huber transition parameter. Default `1.35`
#'   (95 % Gaussian efficiency).
#' @param max_iter Integer. Maximum L-BFGS-B iterations. Default `100`.
#' @param tol Numeric. Gradient-norm tolerance. Default `1e-5`.
#'
#' @return An object of class `c("werm_huber", "werm_fit")`.
#'   Same elements as [werm_least_squares()].
#'
#' @examples
#' # Limit BLAS/OpenMP threads so example CPU time stays proportional to
#' # elapsed time on multicore CRAN machines.
#' Sys.setenv(
#'   OMP_NUM_THREADS = "1",
#'   OPENBLAS_NUM_THREADS = "1",
#'   MKL_NUM_THREADS = "1",
#'   BLAS_NUM_THREADS = "1"
#' )
#'
#' if (nzchar(Sys.getenv("RETICULATE_PYTHON")) &&
#'     weightederm:::.weightederm_examples_available("WERMHuber")) {
#'   set.seed(2)
#'   n <- 12L; p <- 1L; true_cp <- 6L
#'   X <- matrix(rnorm(n * p), n, p)
#'   y <- c(
#'     X[1:true_cp, , drop = FALSE] %*% 3,
#'     X[(true_cp + 1L):n, , drop = FALSE] %*% -3
#'   ) + rnorm(n, sd = 0.03)
#'   fit <- werm_huber(X, y, num_chgpts = 1L, delta = 1L,
#'                     fit_intercept = FALSE, max_iter = 5L, tol = 1e-3)
#'   fit$changepoints
#' }
#'
#' @export
werm_huber <- function(
    X, y,
    num_chgpts,
    delta         = 1L,
    search_method = "efficient",
    fit_intercept = TRUE,
    epsilon       = 1.35,
    max_iter      = 100L,
    tol           = 1e-5,
    penalty       = "none",
    alpha         = 0.0
) {
  py <- .weightederm_py()
  X_py <- .to_numpy_matrix(X)
  y_py <- .to_numpy_vector(y)

  py_model <- py$WERMHuber(
    num_chgpts    = as.integer(num_chgpts),
    delta         = as.integer(delta),
    search_method = search_method,
    fit_intercept = fit_intercept,
    epsilon       = as.numeric(epsilon),
    max_iter      = as.integer(max_iter),
    tol           = as.numeric(tol),
    penalty       = penalty,
    alpha         = as.numeric(alpha)
  )
  py_model$fit(X_py, y_py)

  attrs <- .extract_fixed_attrs(py_model)
  .build_werm_object(attrs, "huber", py_model)
}


#' Fit a WERM changepoint model with binary logistic loss
#'
#' Detects `num_chgpts` changepoints in ordered binary classification data.
#'
#' @inheritParams werm_least_squares
#' @param y Integer or factor vector of binary labels (two unique values).
#' @param max_iter Integer. Maximum L-BFGS-B iterations. Default `100`.
#' @param tol Numeric. Gradient-norm tolerance. Default `1e-5`.
#' @param penalty Character. Default `"l2"` (prevents divergence on separable
#'   segments). Set `"none"` only when segments are not separable.
#' @param alpha Numeric. Default `1.0`.
#'
#' @return An object of class `c("werm_logistic", "werm_fit")`.
#'   Contains all elements from [werm_least_squares()] plus:
#'   \describe{
#'     \item{`classes`}{Character vector of length 2. The two class labels in
#'       sorted order. `classes[2]` is the positive class.}
#'   }
#'
#' @examples
#' # Limit BLAS/OpenMP threads so example CPU time stays proportional to
#' # elapsed time on multicore CRAN machines.
#' Sys.setenv(
#'   OMP_NUM_THREADS = "1",
#'   OPENBLAS_NUM_THREADS = "1",
#'   MKL_NUM_THREADS = "1",
#'   BLAS_NUM_THREADS = "1"
#' )
#'
#' if (nzchar(Sys.getenv("RETICULATE_PYTHON")) &&
#'     weightederm:::.weightederm_examples_available("WERMLogistic")) {
#'   set.seed(3)
#'   n <- 30L; p <- 2L; true_cp <- 15L
#'   X <- matrix(rnorm(n * p), n, p)
#'   eta <- c(
#'     X[1:true_cp, ] %*% c(3, -3),
#'     X[(true_cp + 1L):n, ] %*% c(-3, 3)
#'   )
#'   y <- rbinom(n, 1L, 1 / (1 + exp(-eta)))
#'   fit <- werm_logistic(X, y, num_chgpts = 1L, delta = 3L,
#'                        fit_intercept = FALSE, max_iter = 100L)
#'   fit$changepoints
#'   fit$classes
#' }
#'
#' @export
werm_logistic <- function(
    X, y,
    num_chgpts,
    delta         = 1L,
    search_method = "efficient",
    fit_intercept = TRUE,
    max_iter      = 100L,
    tol           = 1e-5,
    penalty       = "l2",
    alpha         = 1.0
) {
  py <- .weightederm_py()
  X_py <- .to_numpy_matrix(X)
  y_py <- .to_numpy_vector(y)

  py_model <- py$WERMLogistic(
    num_chgpts    = as.integer(num_chgpts),
    delta         = as.integer(delta),
    search_method = search_method,
    fit_intercept = fit_intercept,
    max_iter      = as.integer(max_iter),
    tol           = as.numeric(tol),
    penalty       = penalty,
    alpha         = as.numeric(alpha)
  )
  py_model$fit(X_py, y_py)

  attrs <- .extract_fixed_attrs(py_model)
  attrs$classes <- as.character(reticulate::py_to_r(py_model$classes_))

  .build_werm_object(attrs, "logistic", py_model)
}
