
warnHandler <- function(warnings, ...) {
  get(paste0("warn", warnings), mode = "function")(...)
}
