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
  reticulate::import("weightederm")
  TRUE
}, error = function(e) FALSE)

if (!weightederm_available) {
  message("weightederm Python package not found — skipping all tests.")
}
