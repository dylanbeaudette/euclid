#' Extract vertices and edges from geometries
#'
#' Geometries located in space (all except directions and vectors), have one or
#' more points supporting it. Those can be extracted and modifed with `vert()`.
#' For geometries with a cardinality above one there's a choice of which support
#' point to extract/modify or all. Geometries consisting of more than one vertex
#' (segments, triangles, and tetrahedrons) also have associated edges that can
#' be extracted (but not modified) with `edge()`. `edge_count()` provides the
#' number of edges in each element in the geometry vector. The length of the
#' output of `edge(x)` is `sum(edge_count(x))`.
#'
#' @param x A vector of geometries
#' @param which An integer vector giving the vertex/edge to extract, or `NULL`
#' to extract all.
#' @param value An `euclid_point` vector of the same dimensionality as `x`
#' @param ... arguments passed on to methods
#'
#' @return A `euclid_point` vector for `vert()` or a `euclid_segment` vector for
#' `edge()` matching the dimensionality of `x`
#'
#' @section Vertex definition:
#' For geometries that are defined exclusively by points the definition of the
#' output is straight forward, e.g. for triangles `vert()` will extract one or
#' all of the corners depending on the value of `which`. For the other
#' geometries the output is defined according to the below:
#'
#' - **circles and spheres**: The vertex is the center
#' - **rays**: The vertex is the source
#' - **lines and planes**: The vertex is an arbitrary point on the geometry
#'
#' @family Geometry methods
#' @rdname subgeometries
#' @name subgeometries
#'
#' @examples
#' # Get the source vertex in a segment
#' s <- segment(point(3, 6), point(1, -7))
#' vert(s, 1)
#'
#' # And the target
#' vert(s, 2)
#'
#' # Not providing an index extracts them all
#' vert(s)
#'
#' # Set the source of a segment
#' vert(s, 1) <- point(0, 0)
#' s
#'
#' # Get a point on a line
#' l <- line(4, 7, -1)
#' vert(l)
#'
#' # Setting the vertex of a line moves it so it runs through it
#' point(1, 2) %is_on% l
#' vert(l) <- point(1, 2)
#' point(1, 2) %is_on% l
#'
#' # Get one of the sides from a triangle
#' t <- triangle(point(1, 2), point(6, 3), point(3, 1))
#' edge(t, 2)
#'
#' # or all
#' edge(t)
NULL

#' @rdname subgeometries
#' @export
vert <- function(x, which = NULL, ...) {
  UseMethod("vert")
}
#' @export
vert.euclid_geometry <- function(x, which = NULL, ...) {
  which <- as.integer(which)
  if (any(which > cardinality(x))) {
    cli_abort("{.arg which} cannot be larger than the cardinality of the geometry")
  }
  if (anyNA(which)) {
    # Hack to make NULL appear as value
    cli_abort("{.arg which} must be either {.val {factor('NULL')}} or a vector of finite integers")
  }
  which <- which - 1L
  new_geometry_vector(geometry_vertex(get_ptr(x), which))
}
#' @rdname subgeometries
#' @export
`vert<-` <- function(x, which = NULL, ..., value) {
  UseMethod("vert<-")
}
#' @export
`vert<-.euclid_geometry` <- function(x, which = NULL, ..., value) {
  if (!is_point(value)) {
    cli_abort("New vertices must be given as points")
  }
  if (is_vec(x) || is_direction(x)) {
    cli_abort("You can't set vertices in {.cls euclid_vector} or {.cls euclid_direction} vectors")
  }
  if (dim(x) != dim(value)) {
    cli_abort("{.arg x} and {.arg value} must have the same number of dimensions")
  }
  which <- as.integer(which)
  if (any(which > cardinality(x))) {
    cli_abort("{.arg which} cannot be larger than the cardinality of the geometry")
  }
  if (anyNA(which)) {
    # Hack to make NULL appear as value
    cli_abort("{.arg which} must be either {.val {factor('NULL')}} or a vector of finite integers")
  }
  which <- which - 1L
  new_geometry_vector(geometry_set_vertex(get_ptr(x), which, get_ptr(value)))
}

#' @rdname subgeometries
#' @export
edge <- function(x, which = NULL, ...) {
  UseMethod("edge")
}
#' @export
edge.euclid_geometry <- function(x, which = NULL, ...) {
  which <- as.integer(which)
  if (any(which > edge_count(x))) {
    cli_abort("{.arg which} cannot be larger than the edge count of the geometry")
  }
  which <- which - 1L
  new_geometry_vector(geometry_edges(get_ptr(x), which))
}

#' @rdname subgeometries
#' @export
edge_count <- function(x) {
  UseMethod("edge_count")
}
#' @export
edge_count.euclid_geometry <- function(x) {
  geometry_n_edges(get_ptr(x))
}
