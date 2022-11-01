#' Extract points from geometries
#'
#' Geometries located in space (all except directions and vectors), have one or
#' more points supporting it. Those can be extracted with `vertex()`. For
#' geometries with a cardinality above one there's a choice of which support
#' point to extract.
#'
#' @param x A vector of geometries
#' @param which An integer vector giving the vertex to extract in cases of
#' higher cardinality geometries
#'
#' @return A `euclid_point` vector matching the dimensionality of x
#'
#' @section Vertex definition:
#' For geometries that are defined exclusively by points the definition of the
#' output is straight forward, e.g. for triangles `vertex()` will extract one
#' of the corners depending on the value of `which`. For the other geometries
#' the output is defined according to the below:
#'
#' - **circles and spheres**: The vertex is the center
#' - **rays**: The vertex is the source
#' - **lines and planes**: The vertex is an arbitrary point on the geometry
#'
#' @export
#'
#' @family Geometry methods
#'
#' @examples
#' # Get the source vertex in a segment
#' s <- segment(point(3, 6), point(1, -7))
#' vertex(s, 1)
#'
#' # And the target
#' vertex(s, 2)
#'
#' # Arguments are recycled
#' vertex(s, c(1, 2))
#'
#' # Get a point on a line
#' vertex(line(4, 7, -1))
#'
vertex <- function(x, which = 1L, ...) {
  UseMethod("vertex")
}
#' @export
vertex.euclid_geometry <- function(x, which = 1L, ...) {
  which <- rep_len(as.integer(which), length(x))
  if (any(which > cardinality(x))) {
    cli_abort("{.arg which} cannot be larger than the cardinality of the geometry")
  }
  which <- which - 1L
  new_geometry_vector(geometry_vertex(get_ptr(x), which))
}
#' @rdname vertex
#' @export
`vertex<-` <- function(x, which = 1L, ..., value) {
  UseMethod("vertex<-")
}
#' @export
`vertex<-.euclid_geometry` <- function(x, which = 1L, ..., value) {
  if (!is_point(value)) {
    cli_abort("New vertices must be given as points")
  }
  if (is_vec(x) || is_direction(x)) {
    cli_abort("You can't assing vertices to {.cls euclid_vector} or {.cls euclid_direction} vectors")
  }
  if (dim(x) != dim(value)) {
    cli_abort("{.arg x} and {.arg value} must have the same number of dimensions")
  }
  which <- rep_len(as.integer(which), length(x))
  if (any(which > cardinality(x))) {
    cli_abort("{.arg which} cannot be larger than the cardinality of the geometry")
  }
  which <- which - 1L
  new_geometry_vector(geometry_set_vertex(get_ptr(x), which, get_ptr(value)))
}

#' Vertices and edges of a geometry
#'
#' These functions allows you to extract all the various sub-geometries that a
#' geometry consists of (if any). `vertices()` extracts all the points of each
#' geometry in a vector and `edges()` all the segments. `n_vertices()` gives the
#' number of vertices for each element (equivalent to [cardinality()]), while
#' `n_edges()` gives the number of edges.
#'
#' @param x A vector of geometries
#'
#' @return A `euclid_point` or `euclid_segment` vector with the requested
#' geometries.
#'
#' @family Geometry methods
#'
#' @name subgeometries
#' @rdname subgeometries
#'
#' @examples
#' # not all geometries has both types of subgeometries
#' rect <- iso_rect(point(1, 1), point(3, 6))
#'
#' vertices(rect)
#' edges(rect)
#' n_vertices(rect)
#' n_edges(rect)
#'
NULL

#' @rdname subgeometries
#' @export
vertices <- function(x) {
  UseMethod("vertices")
}
#' @export
vertices.euclid_geometry <- function(x) {
  new_geometry_vector(geometry_vertices(get_ptr(x)))
}

#' @rdname subgeometries
#' @export
edges <- function(x) {
  UseMethod("edges")
}
#' @export
edges.euclid_geometry <- function(x) {
  new_geometry_vector(geometry_edges(get_ptr(x)))
}
#' @rdname subgeometries
#' @export
n_vertices <- function(x) {
  UseMethod("n_vertices")
}
#' @export
n_vertices.euclid_geometry <- function(x) {
  cardinality(x)
}

#' @rdname subgeometries
#' @export
n_edges <- function(x) {
  UseMethod("n_edges")
}
#' @export
n_edges.euclid_geometry <- function(x) {
  new_geometry_vector(geometry_n_edges(get_ptr(x)))
}
