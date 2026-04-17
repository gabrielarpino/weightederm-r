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

  desc_lines <- readLines(file.path(source_root, "DESCRIPTION"), warn = FALSE)
  desc_text <- paste(desc_lines, collapse = "\n")

  expect_match(desc_text, "'weightederm'")
  expect_match(desc_text, "'scikit-learn'")
  expect_match(desc_text, "'reticulate'")
  expect_false(grepl("'WERM'", desc_text, fixed = TRUE))
  expect_match(
    desc_text,
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

  expect_false(any(grepl("\\\\dontrun\\s*\\{", rd_text)))
  expect_false(any(grepl("\\\\donttest\\s*\\{", rd_text)))

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
