testthat::context("MCMAP")

VERBOSE <- FALSE
MCMAP <- function(...) mctools::mcMap(..., verbose = VERBOSE)

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
    MCMAP(3, testFun, errors = "stop")[[1]],
    "all good"
  )

  testthat::expect_error(MCMAP(1, testFun, errors = "stop"))

})

testthat::test_that("Warnings", {

  testFun <- function(i) {
    if (i == 1) warning("some warning")
    else if (i == 2) { warning("first"); warning("second") }
    else if (i == 3) suppressWarnings("nested suppress warning")
    else "all good"
  }

  testthat::expect_equal(MCMAP(1, testFun)[[1]], "some warning")

  testthat::expect_error(MCMAP(1, testFun, warnings = "stop"))

  testthat::expect_equal(
    lapply(attr(MCMAP(2, testFun, warnings = "return"), "warnings")[[1]], `[[`, "message"),
    list("first", "second")
  )

  testthat::expect_equal(
    MCMAP(3, testFun, warnings = "stop")[[1]],
    "nested suppress warning"
  )

  testthat::expect_equal(
    MCMAP(4, testFun, warnings = "stop")[[1]],
    "all good"
  )

})

testthat::test_that("Warnings and Errors", {

  testFun <- function(i) {
    if (i == 1) { warning("first a warning"); stop("then some error") }
    else "all good"
  }

  testthat::expect_is(
    MCMAP(1, testFun)[[1]],
    "error"
  )

  testthat::expect_equal(
    lapply(attr(MCMAP(1, testFun, warnings = "return"), "warnings")[[1]], `[[`, "message"),
    list("first a warning")
  )

  testthat::expect_error(MCMAP(1, testFun, warnings = "stop"))

})

testthat::test_that("Whitelist for Warnings", {

  testFun <- function(i) {
    if (i == 1) warning("catch me if you can")
    else if (i == 2) warning("some warning")
    else "all good"
  }

  testthat::expect_error(
    MCMAP(1:3, testFun, warnings = "stop", warningsWhitelist = "^some warning$")
  )

  testthat::expect_equal(
    MCMAP(2, testFun, warnings = "stop", warningsWhitelist = "^some warning$")[[1]],
    "some warning"
  )

})
