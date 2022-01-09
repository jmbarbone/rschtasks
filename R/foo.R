

#' Wrappers
#'
#' Description of wrapper
#'
#' @param x x param
#' @param y y param
#'
#' @export
wrapper <- function(x, y) {
  r6_class$new(x, y)$get_result()
}


#' @rdname wrapper
#' @export
r6_class <- R6::R6Class(
  "r6_class",
  public = list(

    #' @description makes object
    initialize = function(x, y) {
      .self$result <- x + y
      return(.self)
    },

    #' @field result The result
    result = NA,

    #' @description get result
    #' @param n 1
    get_result = function(n = 1) {
      rep(.self$result, n)
    }
  )
)

# https://github.com/r-lib/roxygen2/issues/1067
