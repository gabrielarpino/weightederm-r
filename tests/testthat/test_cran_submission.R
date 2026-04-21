test_that("CRAN submission requirements stay satisfied", {
  source_root <- test_path("..", "..")
  skip_if_not(
    file.exists(file.path(source_root, "DESCRIPTION")),
    "source DESCRIPTION not available"
  )
  skip_if_not(
    dir.exists(file.path(source_root, "man")),
    "source man directory not available"
  )

  desc_dcf <- read.dcf(file.path(source_root, "DESCRIPTION"))
  description_field <- desc_dcf[1, "Description"]

  expect_match(description_field, "'weightederm'")
  expect_match(description_field, "'scikit-learn'")
  expect_match(description_field, "'reticulate'")
  expect_false(grepl("\\bWERM\\b", description_field))
  expect_false(grepl("\\bchangepoints\\b", description_field))
  # CRAN requires "authors (year) <doi:...>" format in Description.
  expect_match(description_field, "\\(2026\\)")
  expect_match(
    description_field,
    "<doi:10.48550/arXiv.2604.11746>",
    fixed = TRUE
  )

  man_dir <- file.path(source_root, "man")
  rd_files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
  rd_text <- setNames(
    vapply(
      rd_files,
      function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
      character(1)
    ),
    basename(rd_files)
  )

  # All exported topics should avoid \dontrun{} once an example can be made
  # executable in under 5 seconds.
  expect_false(any(grepl("\\\\dontrun\\s*\\{", rd_text)))

  # All werm_* estimator examples must avoid both wrappers and instead use the
  # if (.weightederm_examples_available()) guard.
  werm_rd <- rd_text[grep("^werm_", names(rd_text))]
  expect_false(any(grepl("\\\\donttest\\s*\\{", werm_rd)))

  # CRAN's Debian pretest flagged multithreaded example CPU time for werm_huber.
  # Keep thread limiting in all estimator examples so CPU time stays close to
  # elapsed time on multicore check machines.
  for (topic in names(werm_rd)) {
    expect_match(werm_rd[[topic]], "OMP_NUM_THREADS", info = topic)
    expect_match(werm_rd[[topic]], "OPENBLAS_NUM_THREADS", info = topic)
    expect_match(werm_rd[[topic]], "MKL_NUM_THREADS", info = topic)
    expect_match(werm_rd[[topic]], "BLAS_NUM_THREADS", info = topic)
  }

  expect_match(werm_rd[["werm_least_squares.Rd"]], "n <- 24L", fixed = TRUE)
  expect_match(werm_rd[["werm_least_squares.Rd"]], "true_cp <- 12L", fixed = TRUE)
  expect_match(werm_rd[["werm_huber.Rd"]], "n <- 20L", fixed = TRUE)
  expect_match(werm_rd[["werm_huber.Rd"]], "true_cp <- 10L", fixed = TRUE)
  expect_match(werm_rd[["werm_huber.Rd"]], "delta = 2L", fixed = TRUE)
  expect_match(werm_rd[["werm_huber.Rd"]], "max_iter = 10L", fixed = TRUE)
  expect_match(werm_rd[["werm_logistic.Rd"]], "n <- 30L", fixed = TRUE)
  expect_match(werm_rd[["werm_logistic.Rd"]], "true_cp <- 15L", fixed = TRUE)
  expect_match(werm_rd[["werm_least_squares_cv.Rd"]], "n <- 30L", fixed = TRUE)
  expect_match(werm_rd[["werm_huber_cv.Rd"]], "n <- 24L", fixed = TRUE)
  expect_match(werm_rd[["werm_logistic_cv.Rd"]], "n <- 30L", fixed = TRUE)

  configure_python_rd <- rd_text[["weightederm_configure_python.Rd"]]
  expect_match(configure_python_rd, "Sys\\.which\\(\"python\"\\)")
  expect_match(configure_python_rd, "required = FALSE", fixed = TRUE)

  exported_topics <- c(
    "weightederm_configure_python.Rd",
    "werm_least_squares.Rd",
    "werm_huber.Rd",
    "werm_logistic.Rd",
    "werm_least_squares_cv.Rd",
    "werm_huber_cv.Rd",
    "werm_logistic_cv.Rd",
    "predict.werm_fit.Rd",
    "coef.werm_fit.Rd",
    "summary.werm_fit.Rd"
  )

  for (topic in exported_topics) {
    expect_match(
      rd_text[[topic]],
      "\\\\value\\s*\\{",
      info = topic
    )
  }
})
