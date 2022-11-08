#' @keywords internal
#' @import rlang
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @useDynLib euclid, .registration = TRUE
#' @import cli
## usethis namespace: end
NULL

#' Extension points
#'
#' Euclid is build to be extended. Some of the core functions in the package are
#' generics that other packages can provide methods to, but in some
#' circumstances it was easier to provide a more indirect extension mechanism
#' by providing methods for one or more of the generics below:
#'
#' @param x,y vectors
#' @param ... additional arguments
#'
#' @return Depends on function. Consult the main function to see the expected
#' output (e.g. [centroid()] for `centroid_impl()`)
#'
#' @keywords internal
#' @rdname euclid_extend
#' @name euclid_extend
NULL
