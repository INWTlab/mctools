#' Helpers for deducing options of mcMap
#'
#' Helpers for deducing options of mcMap. A translation of R's warning option
#' into this framework. Logging of warnings and errors is not handled with these
#' options for \code{mcMap}.
#'
#' @param errors,warn (numeric) see \link{options}. '<=1' -> 'suppress';
#'   '>1' -> 'stop'
#' @param whitelist (character) see \link{mcMap}
#'
#' @rdname options
#' @export
getErrorsOption <- function(errors = getOption("mctoolsErrors", 0)) {
  matchOption(errors)
}

#' @rdname options
#' @export
getWarningsOption <- function(warn = getOption("warn", 0)) {
  matchOption(warn)
}

#' @rdname options
#' @export
getWarningsWhitelist <- function(whitelist = getOption("mctoolsWarningsWhitelist", character())) {
  # This function is only here to provide a consistent interface to the options:
  # We have getErrorsOption and getWarningsOption, hence we have this here.
  whitelist
}

matchOption <- function(value) {
  stopifnot(is.numeric(value) && length(value) == 1)
  if (value <= 1) "suppress"
  else "stop"
}
