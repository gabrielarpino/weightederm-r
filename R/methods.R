# ---------------------------------------------------------------------------
# print
# ---------------------------------------------------------------------------

#' @export
print.werm_fit <- function(x, ...) {
  is_cv <- inherits(x, c("werm_least_squares_cv", "werm_huber_cv",
                          "werm_logistic_cv"))
  loss_label <- switch(
    x$.estimator_type,
    least_squares    = "Least Squares",
    least_squares_cv = "Least Squares (CV)",
    huber            = "Huber",
    huber_cv         = "Huber (CV)",
    logistic         = "Logistic",
    logistic_cv      = "Logistic (CV)",
    x$.estimator_type
  )

  cat("WERM Changepoint Estimator —", loss_label, "\n")
  cat(sprintf("  Changepoints : %s (1-indexed)\n",
              if (length(x$changepoints) == 0) "none"
              else paste(x$changepoints, collapse = ", ")))
  cat(sprintf("  num_chgpts   : %d\n", x$num_chgpts))
  cat(sprintf("  num_signals  : %d\n", x$num_signals))
  cat(sprintf("  objective    : %.6g\n", x$objective))

  if (is_cv) {
    cat(sprintf("  best_score   : %.6g\n", x$best_score))
    cat("  cv_results   :\n")
    print(x$cv_results, row.names = FALSE)
  }

  invisible(x)
}


# ---------------------------------------------------------------------------
# predict
# ---------------------------------------------------------------------------

#' Predict using the last-segment refit
#'
#' Evaluates the unweighted base-loss model fitted on the last detected
#' segment. For logistic estimators, returns class labels.
#'
#' @param object A `werm_fit` object returned by any `werm_*` function.
#' @param newdata Numeric matrix of shape `(m, p)`.
#' @param type Character. For logistic estimators: `"class"` (default) or
#'   `"prob"` (returns a `(m, 2)` probability matrix).
#' @param ... Ignored.
#'
#' @return Numeric vector of length `m` (regression) or character vector /
#'   probability matrix (logistic).
#'
#' @export
predict.werm_fit <- function(object, newdata, type = "class", ...) {
  X_py <- .to_numpy_matrix(newdata)
  is_logistic <- inherits(object, c("werm_logistic", "werm_logistic_cv"))

  if (is_logistic && type == "prob") {
    raw <- reticulate::py_to_r(object$.py_model$predict_proba(X_py))
    return(raw)
  }

  raw <- reticulate::py_to_r(object$.py_model$predict(X_py))

  if (is_logistic) {
    as.character(raw)
  } else {
    as.numeric(raw)
  }
}


# ---------------------------------------------------------------------------
# coef
# ---------------------------------------------------------------------------

#' Extract last-segment coefficients
#'
#' Returns the coefficient vector of the unweighted base-loss refit on the
#' last detected segment (the same coefficients used by `predict()`).
#'
#' @param object A `werm_fit` object.
#' @param ... Ignored.
#'
#' @return Named numeric vector of length `p`.
#'
#' @export
coef.werm_fit <- function(object, ...) {
  object$last_segment_coef
}


# ---------------------------------------------------------------------------
# summary
# ---------------------------------------------------------------------------

#' Summarise a fitted WERM model
#'
#' @param object A `werm_fit` object.
#' @param ... Ignored.
#'
#' @return Invisibly returns `object`. Called for its printed side-effect.
#'
#' @export
summary.werm_fit <- function(object, ...) {
  print(object)

  cat("\nLast-segment coefficient(s):\n")
  print(object$last_segment_coef)

  if (!is.null(object$last_segment_intercept)) {
    cat(sprintf("Last-segment intercept: %.6g\n", object$last_segment_intercept))
  }

  cat("\nStage-1 signal coefficients (WERM-weighted, one row per signal):\n")
  print(object$signal_coefs)

  invisible(object)
}
