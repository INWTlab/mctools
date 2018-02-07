#' mclapply with error and warning handlers
#'
#' This is a wrapper around \code{parallel::mclapply}. It supports different
#' options for handling errors and warnings on nodes.
#'
#' @param x (vector) a vector to iterate over
#' @param f (function)
#' @param ... arguments passed to \code{\link[parallel]{mclapply}}
#' @param errors (character) one in \code{c("suppress", "log")}
#' @param warnings (character) one in \code{c("suppress", "log", "asError")}
#' @param warningsWhitelist (character) a vector of regular expressions for white-listing
#'   warnings.
#' @param finallyStop (logical) if we want to raise an error if any of the nodes
#'   produced one.
#'
#' @details
#' This implementation has the same limitations as \code{mclapply}: GUIs (like
#'   RStudio) do not handle console output well. As the documentation of
#'   \code{mclapply} already states: this function is not designed for
#'   interactive sessions.
#'
#' The arguments \code{errors} and \code{warnings} control how errors and
#'   warnings are dealt with. When \code{warnings = "asError"} is specified,
#'   warnings are escalated into errors unless they are on the whitelist, which,
#'   by default, is empty.
#'
#' @return Unlike \code{mclapply} not an object of class 'try-error' is
#'   returned, but a \link{simpleError} in case of errors. This is also the case
#'   when warnings are errors.
#'
#'
#' @rdname mcMap
#' @export
#' @examples
#' mcMap(1:2, function(x) if (x == 2) stop("Don't like '2'!") else x)
#'
mcMap <- function(x, f, ...,
                  errors = getErrorsOption(), warnings = getWarningsOption(),
                  warningsWhitelist = getWarningsWhitelist(),
                  finallyStop = FALSE) {
  force(warnings)
  param <- getOption("warn")
  on.exit(options(warn = param))
  options(warn = 0)
  res <- mclapply(
    x,
    wrapper(f, warnHandler(warnings, warningsWhitelist), errorHandler(errors)),
    ...)
  res <- handleErrors(res, finallyStop)
  res
}

wrapper <- function(fun, warnHandler, errorHandler) {
  force(warnHandler); force(errorHandler)
  function(...) {
    tryCatch(
      error = errorHandler,
      withCallingHandlers(
        warning = warnHandler,
        fun(...)
      ))
  }
}

handleErrors <- function(res, finallyStop) {
  if (finallyStop && any(isError(res))) stop(sprintf(
    "#overall/#errors: %s/%s", length(res), sum(isError(res))))
  else res
}

isError <- function(x) {
  errorTypes <- c("try-error", "simpleError", "error")
  vapply(x, function(x) any(class(x) %in% errorTypes), logical(1))
}
