testthat::context("MCMAP")

MCMAP <- function(..., errors = "suppress") mctools::mcMap(..., errors = errors)

testthat::test_that("Errors", {

  testFun <- function(i) {
    if (i == 1) stop("some error")
    else if (i == 2) { try(stop("nested try-error"), silent = TRUE) }
    else "all good"
  }

  testthat::expect_is(
    MCMAP(1, testFun)[[1]],
    "error"
  )

  testthat::expect_is(
    MCMAP(2, testFun)[[1]],
    "try-error"
  )

  testthat::expect_equal(
    MCMAP(3, testFun, finallyStop = TRUE)[[1]],
    "all good"
  )

  testthat::expect_error(MCMAP(1, testFun, finallyStop = TRUE))

})

testthat::test_that("Warnings", {

  testFun <- function(i) {
    if (i == 1) warning("some warning")
    else if (i == 2) { warning("first"); warning("second") }
    else if (i == 3) suppressWarnings("nested suppress warning")
    else "all good"
  }

  options(warn = -1)

  testthat::expect_equal(MCMAP(1, testFun)[[1]], "some warning")

  testthat::expect_error(MCMAP(1, testFun, warnings = "asError", finallyStop = TRUE))

  testthat::expect_equal(MCMAP(2, testFun)[[1]], "second")

  testthat::expect_equal(
    MCMAP(3, testFun, warnings = "asError")[[1]],
    "nested suppress warning"
  )

  testthat::expect_equal(
    MCMAP(4, testFun, warnings = "asError", finallyStop = TRUE)[[1]],
    "all good"
  )

})

testthat::test_that("Warnings and Errors", {

  testFun <- function(i) {
    if (i == 1) { warning("first a warning"); stop("then some error") }
    else "all good"
  }

  testthat::expect_is(MCMAP(1, testFun)[[1]], "error")

  testthat::expect_equal(
    MCMAP(1, testFun, warnings = "asError")[[1]]$message,
    "Escalated warning: first a warning"
  )

  testthat::expect_error(MCMAP(1, testFun, warnings = "asError", finallyStop = TRUE))

})

testthat::test_that("Whitelist for Warnings", {

  testFun <- function(i) {
    if (i == 1) warning("catch me if you can")
    else if (i == 2) warning("some warning")
    else "all good"
  }

  testthat::expect_error(
    MCMAP(1:3, testFun, warnings = "asError", warningsWhitelist = "^some warning$",
          finallyStop = TRUE)
  )

  testthat::expect_equal(
    MCMAP(2, testFun, warnings = "asError", warningsWhitelist = "^some warning$")[[1]],
    "some warning"
  )

})
