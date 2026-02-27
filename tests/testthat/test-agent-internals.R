context("agent test internals")

# -- Mock helpers --------------------------------------------------------------

mockExpectation <- function(class, msg) {
  structure(
    list(message = msg),
    class = c(class, "expectation", "condition")
  )
}

# Build a minimal object whose as.data.frame() returns a testthat-shaped df.
# Real testthat_results objects are lists (not data frames), so $error at the
# top level returns NULL, not a column value.  We mimic that by wrapping the
# data frame inside a list and providing a custom as.data.frame method.
#
# Each row in `rows` is a list with: file, context, test, nb, failed, skipped,
# error, warning, passed, real, and result (a list of expectation objects).
mockTestthatResults <- function(rows) {
  df <- data.frame(
    file    = vapply(rows, `[[`, "", "file"),
    context = vapply(rows, `[[`, "", "context"),
    test    = vapply(rows, `[[`, "", "test"),
    nb      = vapply(rows, `[[`, 0L, "nb"),
    failed  = vapply(rows, `[[`, 0L, "failed"),
    skipped = vapply(rows, `[[`, FALSE, "skipped"),
    error   = vapply(rows, `[[`, FALSE, "error"),
    warning = vapply(rows, `[[`, 0L, "warning"),
    passed  = vapply(rows, `[[`, 0L, "passed"),
    real    = vapply(rows, `[[`, 0, "real"),
    stringsAsFactors = FALSE
  )
  df$result <- lapply(rows, `[[`, "result")
  structure(list(.df = df), class = "mock_testthat_results")
}

as.data.frame.mock_testthat_results <- function(x, ...) x$.df
registerS3method("as.data.frame", "mock_testthat_results",
                 as.data.frame.mock_testthat_results)


# -- extractDetailedFailures ---------------------------------------------------

test_that("extractDetailedFailures captures expectation_failure", {
  items <- list(
    mockExpectation("expectation_success", "ok"),
    mockExpectation("expectation_failure", "values differ")
  )
  out <- jaspTools:::extractDetailedFailures(items)
  expect_length(out, 1)
  expect_equal(out[[1]]$message, "values differ")
})

test_that("extractDetailedFailures captures expectation_error", {
  items <- list(
    mockExpectation("expectation_error", "object not found")
  )
  out <- jaspTools:::extractDetailedFailures(items)
  expect_length(out, 1)
  expect_equal(out[[1]]$message, "object not found")
})

test_that("extractDetailedFailures returns empty list for non-list input", {
  expect_length(jaspTools:::extractDetailedFailures(NULL), 0)
  expect_length(jaspTools:::extractDetailedFailures("not a list"), 0)
})


# -- extractDetailedWarnings ---------------------------------------------------

test_that("extractDetailedWarnings extracts warning messages", {
  items <- list(
    mockExpectation("expectation_success", "ok"),
    mockExpectation("expectation_warning", "deprecated use")
  )
  out <- jaspTools:::extractDetailedWarnings(items)
  expect_equal(out, "deprecated use")
})

test_that("extractDetailedWarnings deduplicates", {
  items <- list(
    mockExpectation("expectation_warning", "dup"),
    mockExpectation("expectation_warning", "dup")
  )
  expect_length(jaspTools:::extractDetailedWarnings(items), 1)
})


# -- extractSkipReason ---------------------------------------------------------

test_that("extractSkipReason returns reason from expectation_skip", {
  items <- list(
    mockExpectation("expectation_skip", "not on CRAN")
  )
  expect_equal(jaspTools:::extractSkipReason(items), "not on CRAN")
})

test_that("extractSkipReason returns NULL when no skip present", {
  items <- list(mockExpectation("expectation_success", "ok"))
  expect_null(jaspTools:::extractSkipReason(items))
})


# -- buildAgentTestResult: error-only tests are counted ------------------------

test_that("buildAgentTestResult counts test errors in status and summary", {
  mock <- mockTestthatResults(list(
    list(file = "test-foo.R", context = "foo", test = "works",
         nb = 1L, failed = 0L, skipped = FALSE, error = TRUE,
         warning = 0L, passed = 0L, real = 0.5,
         result = list(mockExpectation("expectation_error", "boom")))
  ))

  allResults <- list(myModule = mock)
  res <- jaspTools:::buildAgentTestResult(allResults, tempfile())

  expect_equal(res$status, 1L)
  expect_equal(res$summary$error, 1L)
  expect_equal(nrow(res$failures), 1)
  expect_equal(res$failures$message[1], "boom")
})

test_that("buildAgentTestResult status is 0 when all tests pass", {
  mock <- mockTestthatResults(list(
    list(file = "test-bar.R", context = "bar", test = "passes",
         nb = 1L, failed = 0L, skipped = FALSE, error = FALSE,
         warning = 0L, passed = 1L, real = 0.1,
         result = list(mockExpectation("expectation_success", "ok")))
  ))

  allResults <- list(myModule = mock)
  res <- jaspTools:::buildAgentTestResult(allResults, tempfile())

  expect_equal(res$status, 0L)
  expect_equal(res$summary$fail, 0L)
  expect_equal(res$summary$error, 0L)
  expect_equal(res$summary$pass, 1L)
})


# -- buildAgentTestResult: module-level errors ---------------------------------

test_that("buildAgentTestResult records module-level errors", {
  allResults <- list(
    badModule = list(error = TRUE, errorMessage = "could not load")
  )
  res <- jaspTools:::buildAgentTestResult(allResults, tempfile())

  expect_equal(res$status, 1L)
  expect_equal(res$errorModules[["badModule"]], "could not load")
})


# -- print dedup uses module in key --------------------------------------------

test_that("print deduplicates using module, not just file::test", {
  # Two modules with identical file and test names should both appear
  res <- structure(
    list(
      status  = 1L,
      summary = list(fail = 2L, error = 0L, warn = 0L, skip = 0L,
                      pass = 0L, time = 1.0),
      logFile = "/tmp/test.json",
      failures = data.frame(
        module  = c("modA", "modB"),
        file    = c("test-x.R", "test-x.R"),
        test    = c("it works", "it works"),
        message = c("fail A", "fail B"),
        stringsAsFactors = FALSE
      ),
      warnings     = data.frame(module = character(0), file = character(0),
                                 test = character(0), message = character(0),
                                 stringsAsFactors = FALSE),
      skips        = data.frame(module = character(0), file = character(0),
                                 test = character(0), reason = character(0),
                                 stringsAsFactors = FALSE),
      tests        = data.frame(),
      errorModules = character()
    ),
    class = "jaspAgentTestResults"
  )

  output <- utils::capture.output(print(res))
  # Both modules' failures should appear in the output
  expect_true(any(grepl("modA", output)))
  expect_true(any(grepl("modB", output)))
  expect_true(any(grepl("fail A", output)))
  expect_true(any(grepl("fail B", output)))
})
