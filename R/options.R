#' @details
#'
#' Helpers for deducing options of mcMap. This is so we can set the behaviour in
#' the beginning of a script globally. Each call to \code{mcMap} can override
#' these settings. Logging is enabled by default (for \code{options(warn = 0)}).
#' For \code{options(warn = -1)} we suppress logging and warnings. For
#' \code{options(warn = 2)} warnings are handled as errors.
#'
#' @param errors,warn (numeric) see \link{options}. '-1' -> 'suppress';
#'   '0&1' -> 'log'; '2' -> 'asError'
#' @param whitelist (character) see \link{mcMap}
#'
#' @rdname mcMap
#' @export
getErrorsOption <- function(errors = getOption("mctoolsErrors", 0)) {
  matchOption(errors)
}

#' @rdname mcMap
#' @export
getWarningsOption <- function(warn = getOption("warn", 0)) {
  matchOption(warn)
}

#' @rdname mcMap
#' @export
getWarningsWhitelist <- function(whitelist = getOption("mctoolsWarningsWhitelist", character())) {
  # This function is only here to provide a consistent interface to the options:
  # We have getErrorsOption and getWarningsOption, hence we have this here.
  whitelist
}

matchOption <- function(value) {
  stopifnot(is.numeric(value) && length(value) == 1)
  if (value < 0) "suppress"
  else if (value %in% c(0, 1)) "log" # logging by default
  else "aserror"
}
