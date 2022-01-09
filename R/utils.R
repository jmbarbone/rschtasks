#' @importFrom mark %out%

check_windows <- function() {
  fun <- mark::outer_fun()
  if (!is_windows()) {
    abort(glue("{fun} is only valid for windows machines"))
  }
}

is_windows <- function() {
  .Platform$OS.type == "windows"
}

check_schtasks <- function() {
  if (!fs::file_exists(system2("where", "schtasks", stdout = TRUE, stderr = TRUE))) {
    abort("Could not find schtasks")
  }
}

backslash <- function(x) {
  gsub("/", "\\\\", x)
}

forwardslash <- function(x) {
  gsub("\\\\", "/", x)
}

private <- function(.x, .y, ...) {
  # For some testing
  res <- .x$.__enclos_env__$private[[.y]]

  if (is.function(res)) {
    res(...)
  } else {
    res
  }
}

try_dates <- function(x) {
  as.POSIXlt(
    x,
    tryFormats = c(
      "%Y-%m-%d %H:%M:%OS",
      "%Y/%m/%d %H:%M:%OS",
      # common
      "%m-%d-%Y %H:%M:%OS",
      "%m/%d/%Y %H:%M:%OS",
      "%d-%m-%Y %H:%M:%OS",
      "%d/%m/%Y %H:%M:%OS"
    )
  )
}


fmt_hhmm <- function(x = Sys.time()) {
  stopifnot(inherits(x, "POSIXt"))
  format(x, format = "%H:%M")
}

is_true_false <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}
