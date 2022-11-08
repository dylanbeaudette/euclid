#' Access the exact values that defines the geometries
#'
#' This function gives access to the underlying values defining the geometries
#' in a vector. As such they return the same information as calling
#' [as.matrix()] on a geometry vector except the return value is kept as an
#' exact numeric and that you can extract from single elements if the
#' cardinality of the geometry exceeds 1.
#'
#' @param x A geometry vector
#' @param which Either a name or the index of the definition to extract, as
#' matched to `definition_names(x)`
#' @param element For geometries with a cardinality above 1, which element of
#' the geometry should the definition be extracted for. If `NA` the definition
#' for all elements will be returned and the length of the returned vector will
#' be `sum(cardinality(x))` (matching the return of `as.matrix(x)`)
#' @param i,j The row and column of the cell in the transformation to fetch.
#' @param value An `exact_numeric` vector or an object convertible to one
#' @param ... parameters to pass on
#'
#' @return An exact_numeric vector
#'
#' @export
#'
#' @family Geometry methods
#'
#' @examples
#' # Get squared radius of circle
#' circ <- circle(point(4, 7), 25)
#' def(circ, "r2")
#'
#' # Set r2 to 10
#' def(circ, "r2") <- 10
#' circ
#'
#' # Get all the x values from the source of segments
#' s <- segment(point(sample(10, 4), sample(10, 4)),
#'              point(sample(10, 4), sample(10, 4)))
#' def(s, "x", 1L)
#'
#' # Get y for all subelements
#' def(s, "y")
#'
#' # Extract cell values from transformation matrices
#' m <- affine_rotate(c(pi/2, pi/3))
#' def(m, 1, 2)
#'
def <- function(x, ...) {
  UseMethod("def")
}
#' @rdname def
#' @export
def.euclid_geometry <- function(x, which, element = NA, ...) {
  def_names <- definition_names(x)
  if (length(which) != 1) {
    cli_abort("Can't get more than a single definition at a time")
  }
  if (is.character(which)) {
    index <- match(which, def_names)
    if (is.na(index)) {
      cli_abort(c(
        "{.val {which}} does not name a definition of the geometry",
        i = "Use one of {.or {.val {def_names}}}"
      ))
    }
  } else {
    index <- as.integer(which)
    if (is.na(index)) {
      cli_abort(paste0("{.arg which} must be either a string or a value convertible to a scalar integer"))
    }
  }
  if (index < 1 || index > length(def_names)) {
    cli_abort(c(
      "{.arg which} must reference a definitions for the geometry",
      i = "Use a a positive integer less than or equal to {length(def_names)}"
    ))
  }
  if (!anyNA(element)) {
    element <- rep_len(element, length(x))
    if (any(element > cardinality(x))) {
      cli_abort("{.arg element} must not be larger than the cardinality of the element in the vector")
    }
  } else {
    element <- NA_integer_
  }
  new_exact_numeric(geometry_definition(get_ptr(x), index - 1L, element - 1L))
}
#' @rdname def
#' @export
def.euclid_affine_transformation <- function(x, i, j, ...) {
  n <- max(length(x), length(i), length(j))
  if (length(x) == 1) {
    x <- rep_len(x, n)
  }
  if (length(i) == 1) {
    i <- rep_len(i, n)
  }
  if (length(j) == 1) {
    j <- rep_len(j, n)
  }
  if (length(x) != length(i) || length(x) != length(j)) {
    cli_abort("{.arg x}, {.arg i}, and {.arg j} must be either scalars or of the same length")
  }
  new_exact_numeric(transform_definition(get_ptr(x), as.integer(i) - 1L, as.integer(j) - 1L))
}

#' @rdname def
#' @export
`def<-` <- function(x, ..., value) {
  UseMethod('def<-')
}
#' @rdname def
#' @export
`def<-.euclid_geometry` <- function(x, which, element = NA, ..., value) {
  def_names <- definition_names(x)
  if (length(which) != 1) {
    cli_abort("Can't set more than a single definition at a time")
  }
  if (is.character(which)) {
    index <- match(which, def_names)
    if (is.na(index)) {
      cli_abort(c(
        "{.val {which}} does not name a definition of the geometry",
        i = "Use one of {.or {.val {def_names}}}"
      ))
    }
  } else {
    index <- as.integer(which)
    if (is.na(index)) {
      cli_abort(paste0("{.arg which} must be either a string or a value convertible to a scalar integer"))
    }
  }
  if (index < 1 || index > length(def_names)) {
    cli_abort(c(
      "{.arg which} must reference a definitions for the geometry",
      i = "Use a a positive integer less than or equal to {length(def_names)}"
    ))
  }
  if (!anyNA(element)) {
    element <- rep_len(element, length(x))
    if (any(element > cardinality(x))) {
      cli_abort("{.arg element} must not be larger than the cardinality of the element in the vector")
    }
  } else {
    element <- NA_integer_
  }

  new_geometry_vector(
    geometry_set_definition(get_ptr(x), index - 1L, element - 1L, get_ptr(as_exact_numeric(value)))
  )
}
#' @rdname def
#' @export
`def<-.euclid_affine_transformation` <- function(x, i, j, ..., value) {
  if (i > dim(x) + 1 || j > dim(x) + 1) {
    cli_abort(c(
      '{.arg i} or {.arg j} exceeds the dimensions of the transformation matrix',
      i = 'Use a value less or equal to {dom(x) + 1}'
    ))
  }
  if (i == dim(x) + 1 && j != dim(x) + 1) {
    cli_warn("Ignoring non-diagonal elements in the last row")
  }
  restore_euclid_vector(
    transform_set_definition(
      get_ptr(x),
      rep_len(as.integer(i) - 1L, length(x)),
      rep_len(as.integer(j) - 1L, length(x)),
      get_ptr(as_exact_numeric(value))
    ),
    x
  )
}

#' @rdname def
#' @export
definition_names <- function(x, ...) {
  UseMethod("definition_names")
}
#' @export
definition_names.euclid_geometry <- function(x, ...) {
  geometry_definition_names(get_ptr(x))
}
