# ---------------------------------------------------------------------------
# Input coercion
# ---------------------------------------------------------------------------

#' @noRd
.to_numpy_matrix <- function(X) {
  reticulate::r_to_py(as.matrix(X))
}

#' @noRd
.to_numpy_vector <- function(y) {
  reticulate::r_to_py(as.numeric(y))
}

#' @noRd
.weightederm_module_has_attrs <- function(module, attrs) {
  attrs <- as.character(attrs)

  if (length(attrs) == 0L) {
    return(TRUE)
  }

  py_attrs_present <- all(vapply(
    attrs,
    function(attr) {
      tryCatch(
        reticulate::py_has_attr(module, attr),
        error = function(e) FALSE
      )
    },
    logical(1)
  ))

  if (py_attrs_present) {
    return(TRUE)
  }

  if (is.environment(module)) {
    return(all(vapply(
      attrs,
      exists,
      logical(1),
      where = module,
      inherits = FALSE
    )))
  }

  if (is.list(module)) {
    return(all(attrs %in% names(module)))
  }

  FALSE
}

#' @noRd
.weightederm_examples_available <- function(required_attrs = character(),
                                            module = NULL) {
  if (is.null(module)) {
    if (!reticulate::py_module_available("weightederm")) {
      return(FALSE)
    }

    module <- tryCatch(
      reticulate::import("weightederm", delay_load = FALSE),
      error = function(e) NULL
    )

    if (is.null(module)) {
      return(FALSE)
    }
  }

  .weightederm_module_has_attrs(module, required_attrs)
}

# ---------------------------------------------------------------------------
# Extracting fitted attributes from a Python estimator
# ---------------------------------------------------------------------------

#' @noRd
.py_to_r_nullable <- function(x) {
  r_val <- reticulate::py_to_r(x)
  if (is.null(r_val)) NULL else r_val
}

#' @noRd
.extract_fixed_attrs <- function(py_model) {
  cp_0idx       <- as.integer(reticulate::py_to_r(py_model$changepoints_))
  coef          <- as.numeric(reticulate::py_to_r(py_model$last_segment_coef_))
  intercept_raw <- reticulate::py_to_r(py_model$last_segment_intercept_)

  list(
    changepoints           = cp_0idx + 1L,          # convert to 1-indexed
    num_chgpts             = as.integer(reticulate::py_to_r(py_model$num_chgpts_)),
    num_signals            = as.integer(reticulate::py_to_r(py_model$num_signals_)),
    objective              = as.numeric(reticulate::py_to_r(py_model$objective_)),
    n_features_in          = as.integer(reticulate::py_to_r(py_model$n_features_in_)),
    last_segment_coef      = coef,
    last_segment_intercept = if (is.null(intercept_raw)) NULL
                             else as.numeric(intercept_raw),
    signal_coefs           = reticulate::py_to_r(py_model$`_signal_coefs_`),
    signal_intercepts      = .py_to_r_nullable(py_model$`_signal_intercepts_`),
    theta_hat              = reticulate::py_to_r(py_model$`_theta_hat_`)
  )
}

#' @noRd
.extract_cv_attrs <- function(py_model) {
  base <- .extract_fixed_attrs(py_model)

  bounds_raw <- reticulate::py_to_r(py_model$segment_bounds_)
  # bounds_raw is a list of [start, stop] pairs (0-indexed) — convert to 1-indexed
  bounds_1idx <- lapply(bounds_raw, function(b) c(b[[1]] + 1L, b[[2]]))

  cv_results_raw <- reticulate::py_to_r(py_model$cv_results_)

  c(
    base,
    list(
      best_num_chgpts   = as.integer(reticulate::py_to_r(py_model$best_num_chgpts_)),
      best_index        = as.integer(reticulate::py_to_r(py_model$best_index_)) + 1L,
      best_score        = as.numeric(reticulate::py_to_r(py_model$best_score_)),
      cv_results        = data.frame(
        num_chgpts      = as.integer(cv_results_raw$num_chgpts),
        mean_test_score = as.numeric(cv_results_raw$mean_test_score)
      ),
      num_chgpts_grid   = as.integer(reticulate::py_to_r(py_model$num_chgpts_grid_)),
      segment_bounds    = bounds_1idx,
      segment_coefs     = reticulate::py_to_r(py_model$segment_coefs_),
      segment_intercepts = .py_to_r_nullable(py_model$segment_intercepts_)
    )
  )
}

#' @noRd
.build_werm_object <- function(attrs, estimator_type, py_model) {
  structure(
    c(attrs, list(
      .estimator_type = estimator_type,
      .py_model       = py_model       # keep reference for predict()
    )),
    class = c(paste0("werm_", estimator_type), "werm_fit")
  )
}
