#' Boolean operations on geometries
#'
#' While intersection calculation and queries are the only part of the boolean
#' operators provided by euclid, it provides generics for the remaining ones so
#' that other packages may fill the gap (e.g. polyclid for 2D boolean operations
#' on polygons).
#'
#' @param x,y vectors of geometries
#' @param ... arguments passed on to methods
#'
#' @return Return value will depend on the different implementations
#'
#' @name boolean_operations
#' @rdname boolean_operations
#'
#' @family Boolean operations
#'
#' @importFrom generics union
#' @rawNamespace export(union)
#' @aliases union
#' @usage union(x, y, ...)
#'
#'
NULL

#' @rdname boolean_operations
#' @export
difference <- function(x, y, ...) {
  UseMethod("difference")
}
#' @rdname boolean_operations
#' @export
symmetric_difference <- function(x, y, ...) {
  UseMethod("symmetric_difference")
}
#' @rdname boolean_operations
#' @export
complement <- function(x, ...) {
  UseMethod("complement")
}
