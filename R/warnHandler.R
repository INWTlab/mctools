warnHandler <- function(warnings, ...) {
  stopifnot(warnings %in% c("log", "suppress", "asError"))
  browser()
  get(paste0("warn", tolower(warnings)), envir = environment(), mode = "function")(...)
}

warnlog <- function(...) {
  function(w) {
    futile.logger::flog.warn(w$message)
    invokeRestart("muffleWarning")
  }
}

warnsuppress <- function(...) {
  function(w) {
    invokeRestart("muffleWarning")
  }
}

warnaserror <- function(whitelist, ...) {

  force(whitelist)

  onWhitelist <- function(msg) {
    if (length(whitelist) == 0) FALSE
    else any(unlist(lapply(whitelist, grepl, msg)))
  }

  function(w) {
    if (onWhitelist(w$message)) {
      futile.logger::flog.warn(w$message)
      invokeRestart("muffleWarning")
    }
    else {
      msg <- paste("Escalated warning:", w$message)
      futile.logger::flog.error(msg)
      simpleError(msg, w$call)
    }
  }

}
