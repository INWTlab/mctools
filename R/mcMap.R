#' mclapply with error and warning handlers
#'
#' This is a wrapper around \code{parallel::mclapply}. It supports different
#' options for handling errors and warnings on nodes.
#'
#' @param x (vector) a vector to iterate over
#' @param f (function)
#' @param ... arguments passed to \code{\link[parallel]{mclapply}}
#' @param errors (character) one in \code{c("suppress", "stop")}
#' @param warnings (character) one in \code{c("suppress", "stop", "return")}
#' @param warningsWhitelist (character) a vector of regular expressions for white-listing
#'   warnings
#' @param verbose (logical) if we want to log warnings and errors
#'
#' @details
#' The arguments \code{errors} and \code{warnings} control how errors and
#'   warnings are dealt with. 'suppress' will prevent R from throwing a message
#'   using \link{stop} or \link{warning}. However logging is controlled with
#'   \code{verbose}. 'stop' will throw an error after all nodes have completed -
#'   this is what we want in most production systems. 'return' is valid for
#'   warnings since for errors we always return the error object; just like
#'   mclapply. The option 'return' will add a attribute 'warnings' to the
#'   returned list.
#'
#' The default values are chosen, so this is a drop in replacement for mclapply.
#'
#' @export
#' @examples
#' # vignette("Introduction", package = "mctools")
#' mcMap(1:2, function(x) if (x == 2) stop("Don't like '2'!") else x)
#'
mcMap <- function(x, f, ...,
                  errors = getErrorsOption(), warnings = getWarningsOption(),
                  warningsWhitelist = getWarningsWhitelist(),
                  verbose = TRUE) {
  force(warnings)
  param <- getOption("warn")
  on.exit(options(warn = param))
  options(warn = 0)
  res <- mclapply(x, wrapper(f, verbose), ...)
  res <- handleErrors(res, errors)
  res <- handleWarnings(res, warnings, warningsWhitelist)
  res
}

wrapper <- function(fun, verbose = TRUE) {
  # This is where all the magic happens:
  # wrapper wraps a function, so this function will be applied in a try-catch
  # handler and warnings are collected. We need to return and initialize the
  # 'warnings' memory (database) in the returned function, since the modified
  # function is called on a cluster of nodes. Every node needs its own memory so
  # to speak.
  function(...) {

    warnings <- list()

    warnHandler <- function(w) {
      if (verbose) futile.logger::flog.warn(w$message)
      warnings[[length(warnings) + 1]] <<- w
      invokeRestart("muffleWarning")
    }

    errorHandler <- function(e) {
      if (verbose) futile.logger::flog.error(e$message)
      e
    }

    res <- withCallingHandlers(
      tryCatch(fun(...), error = errorHandler),
      warning = warnHandler
    )

    list(resOrError = res, warnings = warnings)

  }

}

handleErrors <- function(res, errors) {
  if (errors == "stop" && any(isError(res))) stop(sprintf(
    "#overall/#errors: %s/%s", length(res), sum(isError(res))))
  else res
}

isError <- function(x) {
  errorTypes <- c("try-error", "simpleError", "error")
  vapply(
    lapply(x, `[[`, "resOrError"),
    function(x) any(class(x) %in% errorTypes), logical(1)
  )
}

handleWarnings <- function(res, warnings, whitelist) {
  if (warnings == "suppress") defaultOutput(res)
  else if (warnings == "return") outputWithWarnings(res)
  else if (warnings == "stop" && any(hasWarning(res))) checkWhitelistAndStop(res, whitelist)
  else defaultOutput(res)
}

hasWarning <- function(res) {
  vapply(res, function(x) length(x$warnings) > 0, logical(1))
}

defaultOutput <- function(res) {
  lapply(res, function(x) x$resOrError)
}

outputWithWarnings <- function(res) {
  out <- defaultOutput(res)
  attr(out, "warnings") <- lapply(res, function(x) x$warnings)
  out
}

checkWhitelistAndStop <- function(res, whitelist) {
  if (checkWhitelist(res, whitelist)) stop(sprintf(
    "#overall/#warnings: %s/%s", length(res), sum(hasWarning(res))))
  else defaultOutput(res)
}

checkWhitelist <- function(res, whitelist) {
  if (length(whitelist) == 0) return(TRUE)
  warnings <- lapply(res, `[[`, "warnings")
  warnings <- unlist(lapply(warnings, function(x) unlist(lapply(x, `[[`, "message"))))
  !all(unlist(lapply(whitelist, grepl, warnings)))
}
