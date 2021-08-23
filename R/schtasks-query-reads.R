
# x <-  system2("schtasks", "/query /fo list", stdout = TRUE)
# schtasks_query_list_read(x)

schtasks_query_list_read <- function(x) {
  original <- x
  ind <- x == ""
  fs <- cumsum(ind)
  fs <- fs[!ind]
  x <- x[!ind]

  splits <- split(x, fs)
  out <- lapply(
    splits,
    function(i) {
      ss <- strsplit(i, "[:]\\s+")
      as.list(sapply(ss, function(j) set_names(j[2], j[1])))
    }
  )

  ind <- purrr::map_int(out, ~which0(names(.x) == "TaskName"))
  nms <- purrr::map2_chr(out, ind, ~ifelse(.y > 0, .x[[.y]], ""))
  # out <- purrr::map2(out, ind, ~ifelse(.y > 0, .x[-.y], .x))
  out <- purrr::map2(out, ind, ~.x[-.y])
  names(out) <- forwardslash(nms)
  attr(out, "print") <- original
  class(out) <- c("schtasks_list", "list")
  out
}

#' @export
print.schtasks_list <- function(x, n = NULL, ...) {
  px <- attr(x, "print")
  n <- n %||% length(x)
  # TODO add dots and note if n < length(x)
  cat(utils::head(px, n), sep = "\n")
  invisible(x)
}

which0 <- function(x) {
  w <- which(x)
  if (length(w)) {
    w[1L]
  } else {
    0L
  }
}

# x <- system2("schtasks", "/query /fo csv", stdout = TRUE)
# schtasks_query_csv_read(x)

schtasks_query_csv_read <- function(x) {
  # read.csv() is significantly faster than read_csv() :(
  out <- utils::read.csv(text = x, na.strings = "N/A", encoding = "UTF-8")

  # Don't need this nonsense
  out <- out[out$TaskName != "TaskName", ]

  # really make sure utf8
  out[] <- lapply(out, rlang::as_utf8_character)

  # clean up the names a bit
  names(out) <- gsub("[.]", "", names(out))
  out$TaskName <- forwardslash(out$TaskName)

  # Really going to try to get the values correct
  out[[2]] <- try_dates(out[[2]])
  tibble::as_tibble(out)
}

# TODO add schtasks_query_table_read()
