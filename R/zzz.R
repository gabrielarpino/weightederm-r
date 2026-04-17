# Internal module handle — populated on first use via .weightederm_py()
.werm_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' @noRd
.weightederm_py <- function() {
  if (is.null(.werm_env$module)) {
    tryCatch(
      {
        .werm_env$module <- reticulate::import("weightederm", delay_load = FALSE)
      },
      error = function(e) {
        stop(
          "The Python 'weightederm' package could not be imported.\n",
          "Install it with:\n",
          "  pip install weightederm\n",
          "or, if you use the development source:\n",
          "  pip install -e /path/to/weightederm\n\n",
          "To point reticulate at a specific Python / virtual environment call\n",
          "  reticulate::use_python('/path/to/python')\n",
          "before loading this package.\n\n",
          "Original error: ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  .werm_env$module
}

#' Configure which Python environment weightederm uses
#'
#' A thin wrapper around [reticulate::use_python()] that must be called
#' **before** any `werm_*` function if the default Python does not have
#' `weightederm` installed.
#'
#' @param python Path to the Python binary or virtual-environment directory.
#' @param required Passed to [reticulate::use_python()].
#'
#' @return Invisibly returns \code{NULL}. Called for side effects only.
#'
#' @examples
#' if (nzchar(Sys.which("python"))) {
#'   weightederm_configure_python(Sys.which("python"), required = FALSE)
#' }
#'
#' @export
weightederm_configure_python <- function(python, required = TRUE) {
  .werm_env$module <- NULL  # reset cached module
  reticulate::use_python(python, required = required)
  invisible(NULL)
}
