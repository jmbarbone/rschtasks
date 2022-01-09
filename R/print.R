
#' @export
print.rschtasks_call <- function(x, ...) {
  cat(crayon::yellow(x[[1]]), x$args, "\n")
  invisible(x)
}

#' @export
print.rschtasks_result <- function(x, ...) {
  if (length(x)) {
    cat(gsub("^SUCCESS", crayon::green("SUCCESS"), attr(x, "stdout")), "\n")
  } else {
    cat("(no result)\n")
  }

  invisible(x)
}
