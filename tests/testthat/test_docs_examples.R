skip_if_not(weightederm_available, "weightederm Python package not available")

## Extract every ```r block whose first line is "# R_DOCS_TEST: <tag>" and
## return a named list of code strings.
.extract_r_docs_blocks <- function(md_file) {
  lines    <- readLines(md_file, warn = FALSE)
  blocks   <- list()
  in_block <- FALSE
  tag      <- NULL
  buf      <- character(0)

  for (line in lines) {
    if (!in_block) {
      if (grepl("^```[rR]\\s*$", line)) {
        in_block <- TRUE
        tag      <- NULL
        buf      <- character(0)
      }
    } else if (grepl("^```\\s*$", line)) {
      if (!is.null(tag)) blocks[[tag]] <- paste(buf, collapse = "\n")
      in_block <- FALSE
    } else {
      if (is.null(tag)) {
        m <- regmatches(line, regexpr("^#\\s*R_DOCS_TEST:\\s*(\\S+)", line, perl = TRUE))
        if (length(m) > 0L) {
          tag <- sub("^#\\s*R_DOCS_TEST:\\s*", "", m)
        } else {
          in_block <- FALSE   # untagged block — ignore
        }
      } else {
        buf <- c(buf, line)
      }
    }
  }

  blocks
}

## ── Run all tagged blocks ────────────────────────────────────────────────────

test_that("all R_DOCS_TEST blocks in user_guide.md execute without error", {
  md_file <- file.path(
    dirname(dirname(getwd())),   # tests/testthat → tests → weightederm-r
    "docs", "user_guide.md"
  )
  skip_if_not(file.exists(md_file), "docs/user_guide.md not found")

  blocks <- .extract_r_docs_blocks(md_file)
  expect_gt(length(blocks), 0L)   # sanity: we actually found some

  for (tag in names(blocks)) {
    env <- new.env(parent = globalenv())
    result <- tryCatch(
      eval(parse(text = blocks[[tag]]), envir = env),
      error = function(e) e
    )
    expect_false(
      inherits(result, "error"),
      label = sprintf("docs block '%s' must run without error (got: %s)",
                      tag,
                      if (inherits(result, "error")) conditionMessage(result) else "ok")
    )
  }
})

## ── Individual block smoke-tests (named for clear failure messages) ──────────

test_that("docs block 'minimal_workflow' is self-contained and correct", {
  md_file <- file.path(dirname(dirname(getwd())), "docs", "user_guide.md")
  skip_if_not(file.exists(md_file), "docs/user_guide.md not found")
  blocks <- .extract_r_docs_blocks(md_file)
  skip_if_not("minimal_workflow" %in% names(blocks))
  env <- new.env(parent = globalenv())
  expect_no_error(eval(parse(text = blocks[["minimal_workflow"]]), envir = env))
  expect_true(exists("fit", envir = env))
  expect_s3_class(env$fit, "werm_fit")
})

test_that("docs block 'predict_logistic' returns correct shapes", {
  md_file <- file.path(dirname(dirname(getwd())), "docs", "user_guide.md")
  skip_if_not(file.exists(md_file), "docs/user_guide.md not found")
  blocks <- .extract_r_docs_blocks(md_file)
  skip_if_not("predict_logistic" %in% names(blocks))
  env <- new.env(parent = globalenv())
  expect_no_error(eval(parse(text = blocks[["predict_logistic"]]), envir = env))
  expect_equal(length(env$labels), 10L)
  expect_equal(dim(env$probs), c(10L, 2L))
})
