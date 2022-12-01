#' Approximate geometry measures
#'
#' These functions provide approximate measures of length, area, and volume of
#' geometries where relevant. The reason for approximation is that some measures
#' require square root operations or multiplication by π, both operations where
#' exactness is necessarily lost. Not all geometries have meaningful measures,
#' e.g. a direction is dimensionless, a line is without area, and a circle has
#' no volume. Some geometries are infinite in some measures, e.g. a plane has an
#' infinite area. The return value will reflect this.
#'
#' @param x A geometry vector
#'
#' @return A numeric vector
#'
#' @rdname geometry_measures
#' @name geometry_measures
#'
#' @family Measures
#'
#' @examples
#'
#' approx_length(vec(point(1:4, 4:7)))
#' approx_area(circle(point(0, 0), 5:9))
#' approx_volume(sphere(point(0, 0, 0), 5:9))
#'
NULL

#' @rdname geometry_measures
#' @export
approx_length <- function(x) {
  UseMethod("approx_length")
}
#' @export
approx_length.euclid_geometry <- function(x) {
  geometry_approx_length(get_ptr(x))
}
#' @export
approx_length.default <- function(x) {
  cli_abort("{.fn approx_length} is only defined for {.cls euclid_geometry} vectors")
}

#' @rdname geometry_measures
#' @export
approx_area <- function(x) {
  UseMethod("approx_area")
}
#' @export
approx_area.euclid_geometry <- function(x) {
  geometry_approx_area(get_ptr(x))
}
#' @export
approx_area.default <- function(x) {
  cli_abort("{.fn approx_area} is only defined for {.cls euclid_geometry} vectors")
}

#' @rdname geometry_measures
#' @export
approx_volume <- function(x) {
  UseMethod("approx_volume")
}
#' @export
approx_volume.euclid_geometry <- function(x) {
  geometry_approx_volume(get_ptr(x))
}
#' @export
approx_volume.default <- function(x) {
  cli_abort("{.fn approx_volume} is only defined for {.cls euclid_geometry} vectors")
}

#' @rdname geometry_measures
#' @export
approx_radius <- function(x) {
  if (!is_base_geometry(x)) {
    cli_abort("{.fn approx_radius} is only defined for {.cls euclid_geometry} vectors")
  }
  if (!is_circle(x) || !is_sphere(x)) {
    rep(NA_real_, length(x))
  }
  sqrt(as.numeric(def(x, "r2")))
}

#' Calculate distances between geometries
#'
#' The minimum distance between two arbitrary geometries is non-trivial and is
#' only exactly defined for non-circular geometries. `distance_squared()` will
#' return the exact squared distance between geometries with `x` and `y` being
#' recycled to the maximum length of either. `distance_matrix` will return a
#' matrix of distances given as numerics (and thus not exact), with the
#' geometries of `x` in the rows and the geometries of `y` in the columns so
#' that the value of `mat[i, j]` corresponds to the distance between `x[i]` and
#' `y[j]`.
#'
#' @param x,y geometry vectors or bounding boxes
#' @param ... arguments passed on to methods
#'
#' @return A `euclid_exact_numeric` vector for `distance_squared()` and a
#' numeric matrix for `distance_matrix()`
#'
#' @export
#'
#' @family Measures
#'
#' @examples
#' # Calculate distances between lines and rays in 3D
#' p <- point(sample(100, 20), sample(100, 20), sample(100, 20))
#' l <- line(p[1:5], p[6:10])
#' r <- ray(p[11:15], p[16:20])
#'
#' # Pairwise exact distance
#' distance_squared(l, r)
#'
#' # All distances
#' approx_distance_matrix(l, r)
#'
distance_squared <- function(x, y, ...) {
  UseMethod("distance_squared")
}
#' @export
distance_squared.default <- function(x, y, ...) {
  cli_abort("No method available to calculate distances between the given input")
}
#' @export
distance_squared.euclid_geometry <- function(x, y, ...) {
  if (!is_base_geometry(y)) {
    return(distance_squared(y, x, ...))
  }
  if (is_weighted_point(x)) {
    x <- as_point(x)
  }
  if (is_weighted_point(y)) {
    y <- as_point(y)
  }
  new_exact_numeric(geometry_squared_distance(get_ptr(x), get_ptr(y)))
}
#' @rdname distance_squared
#' @export
approx_distance_matrix <- function(x, y, ...) {
  UseMethod("approx_distance_matrix")
}
#' @export
approx_distance_matrix.default <- function(x, y, ...) {
  cli_abort("No method available to calculate distances between the given input")
}
#' @export
approx_distance_matrix.euclid_geometry <- function(x, y, ...) {
  if (!is_base_geometry(y)) {
    return(approx_distance_matrix(y, x, ...))
  }
  if (is_weighted_point(x)) {
    x <- as_point(x)
  }
  if (is_weighted_point(y)) {
    y <- as_point(y)
  }
  geometry_distance_matrix(get_ptr(x), get_ptr(y))
}

#' Calculate angle between geometries
#'
#' Angles cannot be given exactly since vector angle relies on the vector length
#' as well as [acos]. This function calculate the angle between two geometries.
#' This is defined for surfaces, curves, and arrows. If a surface is supplied
#' the angle will be calculated based on the surface normal and modified to
#' match the original geometry.
#'
#' @param x,y Geometry vectors. Only surfaces, curves, and arrows are allowed
#'
#' @return A numeric vector given angular difference in radians
#'
#' @export
#'
#' @family Measures
#'
#' @examples
#' # Angle between two lines
#' approx_angle(line(3, 7, -1), line(-5, 4, 2))
#'
#' # Angle between vector and plane
#' approx_angle(plane(-4, 1, 19, -4), vec(5, -7, 2))
#'
approx_angle <- function(x, y) {
  check_geometry_input(x, y, .name = "approx_angle")
  if (is_volume(x) || is_location(x) || is_volume(y) || is_location(y)) {
    cli_abort(c(
      "{.arg x} and {.arg y} must be curves, surfaces, or arrows",
      i = "see the {.help [help page on geometry classes](euclid::is_volume)} to learn more"
    ))
  }
  if (dim(x) != 2) {
    mod <- 0
    if (is_surface(x)) {
      x <- normal(x)
      mod <- pi/2
    }
    if (is_surface(y)) {
      y <- normal(y)
      if (mod != 0) mod <- pi/2
    }
  }
  x <- as_vec(x)
  y <- as_vec(y)
  acos(as.numeric(x * y / (approx_length(x) * approx_length(y))))
}
