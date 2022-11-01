#' Vector of tetrahedrons
#'
#' A tetrahedron, or triangular pyramid, is a polyhedron consisting of 4
#' triangles between 4 vertices. A tetrahedron splits the euclidean space in two
#' by the plane defined by the first 3 vertices and the positive side being the
#' side that includes the fourth vertex. If a all 4 vertices are coplanar the
#' tetrahedron is considered [degenerate][is_degenerate]. Tetrahedrons only
#' exists in 3 dimensions.
#'
#' @param ... Various input. See the Constructor section.
#' @param x A vector of tetrahedrons or an object to convert to it
#'
#' @return An `euclid_tetrahedron` vector
#'
#' @section Constructors:
#' **3 dimensional tetrahedrons**
#' - Providing four points will construct tetrahedrons in the order the points
#'   are given.
#'
#' @export
#'
#' @family Geometries
#' @family Volumes
#'
#' @examples
#' p <- point(sample(8), sample(8), sample(8))
#' tetrahedron(p[1:2], p[3:4], p[5:6], p[7:8])
#'
tetrahedron <- function(...) {
  inputs <- validate_constructor_input(...)

  if (length(inputs) == 0) {
    return(new_tetrahedron_empty())
  }

  points <- inputs[vapply(inputs, is_point, logical(1))]

  if (length(points) == 4) {
    new_tetrahedron_from_4_points(points[[1]], points[[2]], points[[3]], points[[4]])
  } else {
    cli_abort("Can't construct a {.cls euclid_tetrahedron} vector from the given input")
  }
}
#' @rdname tetrahedron
#' @export
is_tetrahedron <- function(x) inherits(x, "euclid_tetrahedron")


# Conversion --------------------------------------------------------------

#' @rdname tetrahedron
#' @export
as_tetrahedron <- function(x) {
  UseMethod("as_tetrahedron")
}
#' @export
as_tetrahedron.default <- function(x) {
  abort("Can't convert the input to a {.cls euclid_tetrahedron} vector")
}
#' @export
as_tetrahedron.euclid_tetrahedron <- function(x) x

# Misc --------------------------------------------------------------------

#' @export
seq.euclid_tetrahedron <- function(from, to, length.out = NULL, along.with = NULL, ...) {
  tetrahedron(
    seq(vertex(from, 1), vertex(to, 1), length.out, along.with),
    seq(vertex(from, 2), vertex(to, 2), length.out, along.with),
    seq(vertex(from, 3), vertex(to, 3), length.out, along.with),
    seq(vertex(from, 4), vertex(to, 4), length.out, along.with)
  )
}

# Internal Constructors ---------------------------------------------------

new_tetrahedron_empty <- function() {
  new_geometry_vector(create_tetrahedron_empty())
}
new_tetrahedron_from_4_points <- function(p, q, r, s) {
  new_geometry_vector(create_tetrahedron_4points(get_ptr(p), get_ptr(q), get_ptr(r), get_ptr(s)))
}
