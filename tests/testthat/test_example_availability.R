test_that("example availability requires the requested Python exports", {
  module <- new.env(parent = emptyenv())
  module$WERMLeastSquares <- TRUE
  module$WERMLogistic <- TRUE

  expect_true(
    weightederm:::.weightederm_module_has_attrs(module, "WERMLeastSquares")
  )
  expect_false(
    weightederm:::.weightederm_module_has_attrs(
      module,
      c("WERMLeastSquares", "WERMHuber")
    )
  )
  expect_true(
    weightederm:::.weightederm_examples_available(
      "WERMLeastSquares",
      module = module
    )
  )
  expect_false(
    weightederm:::.weightederm_examples_available(
      "WERMHuber",
      module = module
    )
  )
})

test_that("example availability recognises attributes on a live Python module", {
  source_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
  python_path <- file.path(
    dirname(source_root),
    "weightederm", ".venv", "bin", "python"
  )
  skip_if_not(file.exists(python_path), "project Python virtualenv not found")

  reticulate::use_python(python_path, required = TRUE)
  skip_if_not(
    reticulate::py_module_available("weightederm"),
    "weightederm Python package not available"
  )

  module <- reticulate::import("weightederm", delay_load = FALSE)

  expect_true(
    weightederm:::.weightederm_module_has_attrs(
      module,
      c("WERMLeastSquares", "WERMHuber", "WERMLogistic")
    )
  )
  expect_true(
    weightederm:::.weightederm_examples_available(
      c("WERMLeastSquares", "WERMHuber", "WERMLogistic"),
      module = module
    )
  )
})
