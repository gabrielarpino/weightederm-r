## Limit numpy/BLAS threading so that CPU time stays proportional to
## elapsed time on multi-core CRAN check machines (avoids the
## "CPU time > 2.5 times elapsed" NOTE on Debian).
Sys.setenv(
  OMP_NUM_THREADS    = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS    = "1",
  BLAS_NUM_THREADS   = "1"
)

## Point reticulate at the project venv that has weightederm installed.
## Skip all tests if Python / weightederm is unavailable.
## During testthat execution, getwd() is <pkg>/tests/testthat/.
## We need to go up 3 levels: tests/testthat/ → tests/ → weightederm-r/ → Weighted-ERM packages/
PYTHON_PATH <- file.path(
  dirname(dirname(dirname(getwd()))),
  "weightederm", ".venv", "bin", "python"
)

if (file.exists(PYTHON_PATH)) {
  reticulate::use_python(PYTHON_PATH, required = TRUE)
} else {
  # Fall back: let reticulate find whatever Python it can
}

weightederm_available <- tryCatch({
  weightederm:::.weightederm_examples_available(
    c(
      "WERMLeastSquares",
      "WERMLeastSquaresCV",
      "WERMHuber",
      "WERMHuberCV",
      "WERMLogistic",
      "WERMLogisticCV"
    )
  )
}, error = function(e) FALSE)

if (!weightederm_available) {
  message("compatible weightederm Python package not found — skipping all tests.")
}
