geometry_math_cumsum <- function(x) {
  UseMethod("geometry_math_cumsum")
}
#' @export
geometry_math_cumsum.default <- function(x) {
  cli_abort("{.cls {class(x)[1]}} does not support {.fn cumsum}")
}
geometry_math_cumprod <- function(x) {
  UseMethod("geometry_math_cumprod")
}
#' @export
geometry_math_cumprod.default <- function(x) {
  cli_abort("{.cls {class(x)[1]}} does not support {.fn cumprod}")
}
geometry_math_cummin <- function(x) {
  UseMethod("geometry_math_cummin")
}
#' @export
geometry_math_cummin.default <- function(x) {
  cli_abort("{.cls {class(x)[1]}} does not support {.fn cummin}")
}
geometry_math_cummax <- function(x) {
  UseMethod("geometry_math_cummax")
}
#' @export
geometry_math_cummax.default <- function(x) {
  cli_abort("{.cls {class(x)[1]}} does not support {.fn cummax}")
}

#' @export
Math.euclid_geometry <- function(x, ...) {
  res <- switch(.Generic,
    "cumsum" = geometry_math_cumsum(x),
    "cumprod" = geometry_math_cumprod(x),
    "cummin" = geometry_math_cummin(x),
    "cummax" = geometry_math_cummax(x)
  )
  restore_euclid_vector(res, x)
}
