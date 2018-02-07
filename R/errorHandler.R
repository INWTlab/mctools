errorHandler <- function(errors, ...) {
  stopifnot(errors %in% c("log", "suppress"))
  get(paste0("error", errors), mode = "function")(...)
}

errorlog <- function(...) {
  function(e) {
    futile.logger::flog.error(e$message)
    e
  }
}

errorsuppress <- function(...) {
  identity
}
