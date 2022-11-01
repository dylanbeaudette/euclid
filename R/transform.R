#' Create affine transformation matrices
#'
#' These functions allow you to create vectors of transformation matrices for
#' affine transformation in 2 or 3 dimensions. Transformation matrices are used
#' to apply transformations to geometries using [transform()]. Transformations
#' can be stacked by multiplying them together. This is generally more
#' performant than applying transformations one by one to geometries.
#' Transformations can be reversed by applying the inverse transformation to a
#' geometry. The inverse transformation matrix can be obtained using
#' `inverse()`. Affine transformation matrices have an additional column and row
#' compared to linear transformation matrices. This means that matrices for 2
#' dimensional transformations are 3x3 and matrices for 2 dimensional
#' transformations are 4x4. In both cases the last row will consist of 0's and a
#' final scaling factor (usually 1). Rotation is generally not possible to do
#' while maintaining exactness as sine and cosine cannot be calculate to
#' exactness. 3 dimensional rotation can either be done around an axis, around
#' a direction, or be defining specific angles of rotation for yaw, pitch, and
#' roll.
#'
#' @note Circles and spheres only transforms correctly with euclidean
#' transformations (i.e. translation, rotations, and reflection) as well as
#' scaling by the same factor along all dimensions (as provided by
#' `affine_scaling()`). Shearing and stretching/squeezing will only affect the
#' location of the center, not the circularity of the geometry.
#'
#' @param dim The dimensionality of the transformation matrix
#' @param mat An object that can be converted to an affine transformation matrix
#' vector. Matrices and arrays can be converted provided they have the correct
#' dimensions. List of matrices can be converted provided that all matrices have
#' the same dimensions and that the dimensions is correct
#' @param vec A vector of vectors or an object convertible to one
#' @param fac A scaling factor to apply
#' @param x,y,z,xy,xz,yx,yz,zx,zy Scaling and shearing factors for each separate
#' dimension/plane, or flags to indicate whether to reflect on the given axis
#' @param rho An angle in radians to rotate (counterclockwise)
#' @param axis For 3 dimensional rotation, which axis to rotate around
#' @param direction A direction vector or an object convertible to one
#' @param yaw,pitch,roll Angles in radians for yaw, pitch, and roll rotation.
#'
#' @return An `euclid_affine_transformation` vector
#'
#' @rdname affine_transformation
#' @name affine_transformation
#'
#' @family non-geometry vectors
#'
#' @examples
#' # Rotate triangle around its centroid and then around the center
#' p <- point(sample(10, 3), sample(10, 3))
#' t <- triangle(p[1], p[2], p[3])
#'
#' ct <- centroid(t)
#' # Assemble transformation (remember reverse order)
#' trans <- affine_rotate(pi/4) *
#'   affine_translate(vec(ct)) *
#'   affine_rotate(2*pi/5) *
#'   affine_translate(-vec(ct))
#'
#' t2 <- transform(t, trans)
#'
#' plot(c(t, t2), col = c("grey", "firebrick"), border = NA)
#'
NULL

#' @rdname affine_transformation
#' @export
affine_identity <- function(dim = 2L) {
  if (dim == 2) {
    new_affine_transformation2(create_transform_2_identity(1L))
  } else {
    new_affine_transformation3(create_transform_3_identity(1L))
  }
}
#' @rdname affine_transformation
#' @export
affine_matrix <- function(mat) {
  if (is.matrix(mat)) {
    mat <- list(mat)
  } else if (is.array(mat)) {
    x_dim <- dim(mat)
    if (length(dim) == 2) {
      mat <- list(as.matrix(mat))
    } else {
      if (length(x_dim) != 3) {
        cli_abort("Only 3-dimensional arrays can be converted to a {.cls euclid_affine_transformation} vector")
      }
      m_size <- prod(x_dim[1:2])
      n <- length(mat) / m_size
      mat <- split(mat, rep(n, each = m_size))
      mat <- lapply(mat, matrix, nrow = x_dim[1], ncol = x_dim[2])
    }
  }
  if (!all(vapply(mat, is.matrix, logical(1)) || vapply(mat, anyNA, logical(1)))) {
    cli_abort("{.arg x} must be a matrix, a list of matrices, a 3-dimensional array, or an object convertible to one of these")
  }
  dimensionality <- max(vapply(mat, ncol, integer(1)))
  mat <- lapply(mat, function(x) {
    mode(x) <- "numeric"
    if (anyNA(x)) {
      return(matrix(NA_real_, ncol = dimensionality, nrow = dimensionality))
    }
    n_cols <- ncol(x)
    if (n_cols != dimensionality) {
      cli_abort("Cannot provide a mix of dimensionalities")
    }
    if (n_cols != 3 && n_cols != 4) {
      cli_abort("Only 2 and 3 dimensional transformation matrices supported")
    }
    if (nrow(x) == ncol(x) - 1) {
      x <- rbind(x, 0)
      x[length(x)] <- 1
    }
    if (ncol(x) != nrow(x)) {
      cli_abort("Malformed transformation matrix")
    }
    x
  })
  if (dimensionality == 3) {
    new_affine_transformation2(create_transform_2_matrix(mat))
  } else {
    new_affine_transformation3(create_transform_3_matrix(mat))
  }
}
#' @rdname affine_transformation
#' @export
affine_translate <- function(vec) {
  vec <- as_vec(vec)
  if (dim(vec) == 2) {
    new_affine_transformation2(create_transform_2_translate(get_ptr(vec)))
  } else {
    new_affine_transformation3(create_transform_3_translate(get_ptr(vec)))
  }
}
#' @rdname affine_transformation
#' @export
affine_scale <- function(fac, dim = 2L) {
  fac <- as_exact_numeric(fac)
  if (dim == 2) {
    new_affine_transformation2(create_transform_2_scale(get_ptr(fac)))
  } else {
    new_affine_transformation3(create_transform_3_scale(get_ptr(fac)))
  }
}
#' @rdname affine_transformation
#' @export
affine_scale2 <- function(x = 1, y = 1, z = NA, dim = NA) {
  x <- as_exact_numeric(x)
  y <- as_exact_numeric(y)
  if (!is.na(z)) dim <- 3
  if (is.na(dim)) dim <- 2
  if (dim == 2) {
    new_affine_transformation2(create_transform_2_scale2(get_ptr(x), get_ptr(y)))
  } else {
    z <- as_exact_numeric(if (is.na(z)) 1 else z)
    new_affine_transformation3(create_transform_3_scale2(get_ptr(x), get_ptr(y), get_ptr(z)))
  }
}
#' @rdname affine_transformation
#' @export
affine_shear <- function(x = NA, y = NA, xy = NA, xz = NA, yx = NA, yz = NA, zx = NA, zy = NA) {
  if (!is.na(x) || !is.na(y)) {
    x <- as_exact_numeric(if (is.na(x)) 0 else x)
    y <- as_exact_numeric(if (is.na(y)) 0 else y)
    new_affine_transformation2(create_transform_2_shear(get_ptr(x), get_ptr(y)))
  } else {
    xy <- as_exact_numeric(if (is.na(xy)) 0 else xy)
    xz <- as_exact_numeric(if (is.na(xz)) 0 else xz)
    yx <- as_exact_numeric(if (is.na(yx)) 0 else yx)
    yz <- as_exact_numeric(if (is.na(yz)) 0 else yz)
    zx <- as_exact_numeric(if (is.na(zx)) 0 else zx)
    zy <- as_exact_numeric(if (is.na(zy)) 0 else zy)
    new_affine_transformation3(create_transform_3_shear(get_ptr(xy), get_ptr(xz), get_ptr(yx), get_ptr(yz), get_ptr(zx), get_ptr(zy)))
  }
}
#' @rdname affine_transformation
#' @export
affine_reflect <- function(x = FALSE, y = FALSE, z = FALSE, dim = 2L) {
  x <- as_exact_numeric(if (x) -1 else 1)
  y <- as_exact_numeric(if (y) -1 else 1)
  z <- as_exact_numeric(if (z) -1 else 1)
  if (dim == 2) {
    new_affine_transformation2(create_transform_2_scale2(get_ptr(x), get_ptr(y)))
  } else {
    new_affine_transformation3(create_transform_3_scale2(get_ptr(x), get_ptr(y), get_ptr(z)))
  }
}
#' @rdname affine_transformation
#' @export
affine_rotate <- function(rho, axis = NA, direction = NA, yaw = NA, pitch = NA, roll = NA) {
  if (isTRUE(is.na(axis)) && isTRUE(is.na(direction)) && isTRUE(is.na(yaw)) && isTRUE(is.na(pitch)) && isTRUE(is.na(roll))) {
    rho <- as_exact_numeric(rho)
    new_affine_transformation2(create_transform_2_rotate(get_ptr(rho)))
  } else {
    if (!isTRUE(is.na(axis))) {
      affine_rotate_axis(rho, axis)
    } else if (!isTRUE(is.na(direction))) {
      affine_rotate_direction(rho, direction)
    } else {
      affine_rotate_ypr(yaw, pitch, roll)
    }
  }
}
#' @rdname affine_transformation
#' @export
is_affine_transformation <- function(x) inherits(x, "euclid_affine_transformation")

# Conversion --------------------------------------------------------------

#' @rdname affine_transformation
#' @export
as_affine_transformation <- function(x) {
  UseMethod("as_affine_transformation")
}
#' @export
as_affine_transformation.euclid_affine_transformation <- function(x) {
  x
}
#' @export
as_affine_transformation.arrays <- function(x) {
  affine_matrix(x)
}
#' @export
as_affine_transformation.list <- function(x) {
  affine_matrix(x)
}
#' @export
as.array.euclid_affine_transformation <- function(x, ...) {
  transform_to_array(get_ptr(x))
}
#' @export
as.character.euclid_affine_transformation <- function(x, ...) {
  format(x, ...)
}
#' @export
as.list.euclid_affine_transformation <- function(x, ...) {
  array <- transform_to_array(get_ptr(x))
  x_dim <- dim(array)
  m_size <- prod(x_dim[1:2])
  n <- length(array) / m_size
  x <- split(array, rep(n, each = m_size))
  lapply(x, matrix, nrow = x_dim[1], ncol = x_dim[2])
}

# Vector basic ------------------------------------------------------------

#' @export
format.euclid_affine_transformation <- function(x, ...) {
  transform_format(get_ptr(x))
}
#' @export
print.euclid_affine_transformation <- function(x, ...) {
  cat("<", dim(x), "D affine transformation matrices [", length(x), "]>\n", sep = "")
  if (length(x) == 0) {
    cat("[empty]")
  } else {
    print(format(x, ...), quote = FALSE)
  }
  invisible(x)
}
#' @importFrom utils str
#' @export
str.euclid_affine_transformation <- function(object, ...) {
  show <- min(5, length(object))
  cat(
    "affine transformation{", dim(object), "}",
    if (length(object) == 0) " [0]" else paste0(" [1:", length(object), "] "),
    if (length(object) == 0) "" else paste(format(object)[seq_len(show)], collapse = " "),
    if (show < length(object)) " ..." else "",
    sep = ""
  )
  invisible(object)
}
#' @export
length.euclid_affine_transformation <- function(x) {
  transform_length(get_ptr(x))
}
#' @export
rep.euclid_affine_transformation <- function(x, ...) {
  index <- rep(seq_along(x), ...)
  x[index]
}
#' @export
dim.euclid_affine_transformation <- function(x) {
  transform_dimension(get_ptr(x))
}
#' @export
`[.euclid_affine_transformation` <- function(x, i, j, ..., drop = TRUE) {
  index <- seq_along(x)[i]
  restore_euclid_vector(transform_subset(get_ptr(x), index), x)
}
#' @export
`[[.euclid_affine_transformation` <- function(x, i) {
  if (length(i) != 1) {
    cli_abort("attempt to select more than one element in vector")
  }
  x[i]
}
#' @export
`[<-.euclid_affine_transformation` <- function(x, i, j, ..., value) {
  if (is.numeric(i) && all(i >= 0)) {
    index <- seq_len(max(i))[i]
  } else {
    index <- seq_along(x)[i]
  }
  if (length(index) == 0) {
    return(x)
  }
  if (anyNA(index)) {
    cli_abort("Trying to assign to non-existing element")
  }
  value <- rep_len(as_affine_transformation(value), length(index))
  restore_euclid_vector(transform_assign(get_ptr(x), index, get_ptr(value)), x)
}
#' @export
`[[<-.euclid_affine_transformation` <- function(x, i, value) {
  if (length(i) != 1) {
    cli_abort("attempt to assign to more than one element in vector")
  }
  x[i] <- value
  x
}
#' @export
`$.euclid_affine_transformation` <- function(x, name) {
  cli_abort("{.code $} is not defined for {.cls euclid_affine_transformation} vectors")
}
#' @export
`$<-.euclid_affine_transformation` <- function(x, name, value) {
  cli_abort("{.code $<-} is not defined for {.cls euclid_affine_transformation} vectors")
}
#' @export
c.euclid_affine_transformation <- function(..., recursive = FALSE) {
  input <- lapply(list(...), as_affine_transformation)
  if (length(unique(vapply(input, dim, integer(1)))) != 1) {
    cli_abort("Transformations can only be combined with other transformations of the same dimensionalities")
  }
  input <- lapply(input, get_ptr)
  res <- transform_combine(input[[1]], input[-1])
  restore_euclid_vector(res, ..1)
}
#' @export
is.na.euclid_affine_transformation <- function(x) {
  transform_is_na(get_ptr(x))
}
#' @export
`is.na<-.euclid_affine_transformation` <- function(x, value) {
  x[is.na(x)] <- value
  x
}
#' @export
anyNA.euclid_affine_transformation <- function(x, recursive) {
  transform_any_na(get_ptr(x))
}
#' @export
as.data.frame.euclid_affine_transformation <- function(x, row.names = NULL, optional = FALSE, ...) {
  df <- list(x)
  class(df) <- "data.frame"
  attr(df, "row.names") <- .set_row_names(length(x))
  df
}
#' @export
unique.euclid_affine_transformation <- function(x, incomparables = FALSE, ...) {
  restore_euclid_vector(transform_unique(get_ptr(x)), x)
}
#' @export
duplicated.euclid_affine_transformation <- function(x, incomparables = FALSE, ...) {
  transform_duplicated(get_ptr(x))
}
#' @export
anyDuplicated.euclid_affine_transformation <- function(x, incomparables = FALSE, ...) {
  transform_any_duplicated(get_ptr(x))
}
match_transform <- function(x, table) {
  transform_match(get_ptr(x), get_ptr(table))
}

# Misc --------------------------------------------------------------------

#' @rdname affine_transformation
#' @export
inverse <- function(x) {
  if (!is_affine_transformation(x)) {
    cli_abort("{.arg x} must be an affine transformation vector")
  }
  restore_euclid_vector(transform_inverse(get_ptr(x)), x)
}
#' @rdname affine_transformation
#' @export
is_reflecting <- function(x) {
  if (!is_affine_transformation(x)) {
    cli_abort("{.arg x} must be an affine transformation vector")
  }
  transform_is_reflecting(get_ptr(x))
}

# Group generics ----------------------------------------------------------

#' @export
Ops.euclid_affine_transformation <- function(e1, e2) {
  if (!.Generic %in% c("*", "==", "!=")) {
    cli_abort("The {.code {.Generic}} operator is not defined for {.cls euclid_affine_transformation} vectors")
  }
  e1 <- as_affine_transformation(e1)
  e2 <- as_affine_transformation(e2)
  if (dim(e1) != dim(e2)) {
    cli_abort("transformation matrices must be of the same dimensionality")
  }
  if (length(e1) == 0 || length(e2) == 0) {
    new_affine_transformation_empty(dim(e1))
  }
  res <- switch(.Generic,
    "*" = transform_multiply(get_ptr(e1), get_ptr(e2)),
    "==" = transform_is_equal(get_ptr(e1), get_ptr(e2)),
    "!=" = !transform_is_equal(get_ptr(e1), get_ptr(e2)),
  )
  if (.Generic %in% "*") {
    res <- restore_euclid_vector(res, e1)
  }
  res
}
#' @export
Math.euclid_affine_transformation <- function(x, ...) {
  if (.Generic != "cumprod") {
    cli_abort("{.fn {.Generic}} is not defined for transformation matrices")
  }
  restore_euclid_vector(transform_cumprod(get_ptr(x)), x)
}
#' @export
Summary.euclid_affine_transformation <- function(..., na.rm) {
  na.rm = isTRUE(na.rm)
  input <- do.call(c, list(...))
  if (.Generic != "prod") {
    cli_abort("{.fn {.Generic}} is not defined for transformation matrices")
  }
  restore_euclid_vector(transform_prod(get_ptr(input), na.rm), input)
}

# Internal constructors ---------------------------------------------------

affine_rotate_axis <- function(rho, axis) {
  rho <- as.numeric(rho)
  n <- max(length(rho), length(axis))
  sinr <- sin(rho)
  cosr <- cos(rho)
  if (length(rho) == 1) {
    sinr <- rep_len(sinr, n)
    cosr <- rep_len(cosr, n)
  }
  if (length(axis) == 1) {
    axis <- rep_len(axis, n)
  }
  if (length(cosr) != length(axis)) {
    cli_abort("{.arg rho} and {.arg axis} must be either scalar or equal length")
  }
  is_na <- !is.finite(cosr) || !is.finite(sinr) || is.na(axis)
  matrices <- lapply(seq_len(n), function(i) {
    if (is_na[i]) {
      return(matrix(NA_real_, ncol = 4, nrow = 4))
    }
    switch(axis[i],
      x = matrix(
        c(1,       0,        0, 0,
          0, cosr[i], -sinr[i], 0,
          0, sinr[i],  cosr[i], 0,
          0,       0,        0, 1),
        nrow = 4, ncol = 4
      ),
      y = matrix(
        c( cosr[i], 0, sinr[i], 0,
                 0, 1,       0, 0,
          -sinr[i], 0, cosr[i], 0,
                 0, 0,       0, 1),
        nrow = 4, ncol = 4
      ),
      z = matrix(
        c(cosr[i], -sinr[i], 0, 0,
          sinr[i],  cosr[i], 0, 0,
                0,        0, 1, 0,
                0,        0, 0, 1),
        nrow = 4, ncol = 4
      ),
      cli_abort("Unknown axis: {.val {axis[i]}}")
    )
  })
  new_affine_transformation3(create_transform_3_matrix(matrices))
}

affine_rotate_direction <- function(rho, direction) {
  direction <- as_vec(direction)
  if (dim(direction) != 3) {
    cli_abort("Need a 3-dimensional vector for affine rotation around vector")
  }
  n <- max(length(rho), length(direction))
  rho <- as.numeric(rho)
  sinr <- sin(rho)
  cosr <- cos(rho)
  if (length(rho) == 1) {
    sinr <- rep_len(sinr, n)
    cosr <- rep_len(cosr, n)
  }
  if (length(direction) == 1) {
    direction <- rep_len(direction, n)
  }
  if (length(cosr) != length(direction)) {
    cli_abort("{.arg rho} and {.arg direction} must be either scalar or equal length")
  }
  is_na <- !is.finite(cosr) || !is.finite(sinr) || is.na(direction)
  direction <- as.matrix(direction)
  l <- sqrt(direction[,1]^2 + direction[,2]^2 + direction[,3]^2)
  direction <- direction / cbind(l, l, l)
  m11 <- cosr + direction[,1]^2 * (1 - cosr)
  m12 <- direction[,1] * direction[,2] * (1 - cosr) - direction[,3]*sinr
  m13 <- direction[,1] * direction[,3] * (1 - cosr) + direction[,2]*sinr
  m21 <- direction[,1] * direction[,2] * (1 - cosr) + direction[,3]*sinr
  m22 <- cosr + direction[,2]^2 * (1 - cosr)
  m23 <- direction[,2] * direction[,3] * (1 - cosr) - direction[,1]*sinr
  m31 <- direction[,1] * direction[,3] * (1 - cosr) - direction[,2]*sinr
  m32 <- direction[,2] * direction[,3] * (1 - cosr) + direction[,1]*sinr
  m33 <- cosr + direction[,3]^2 * (1 - cosr)
  matrices <- lapply(seq_len(n), function(i) {
    if (is_na[i]) {
      return(matrix(NA_real_, ncol = 4, nrow = 4))
    }
    matrix(c(
      m11, m12, m13, 0,
      m21, m22, m23, 0,
      m31, m32, m33, 0,
        0,   0,   0, 1
    ), ncol = 4, nrow = 4)
  })
  new_affine_transformation3(create_transform_3_matrix(matrices))
}

affine_rotate_ypr <- function(yaw, pitch, roll) {
  yaw <- as.numeric(yaw)
  cosy <- cos(yaw)
  siny <- sin(yaw)
  pitch <- as.numeric(pitch)
  cosp <- cos(pitch)
  sinp <- sin(pitch)
  roll <- as.numeric(roll)
  cosr <- cos(roll)
  sinr <- sin(roll)
  n <- max(length(yaw), length(pitch), length(roll))
  if (length(yaw) == 1) {
    cosy <- rep_len(cosy, n)
    siny <- rep_len(siny, n)
  }
  if (length(pitch) == 1) {
    cosp <- rep_len(cosp, n)
    sinp <- rep_len(sinp, n)
  }
  if (length(roll) == 1) {
    cosr <- rep_len(cosr, n)
    sinr <- rep_len(sinr, n)
  }
  if (length(cosy) != length(cosp) || length(cosy) != length(cosr)) {
    cli_abort("{.arg yaw}, {.arg pitch}, and {.arg roll} must be either scalar or equal length")
  }
  is_na <- !is.finite(cosy) || !is.finite(siny) || !is.finite(cosp) || !is.finite(sinp) || !is.finite(cosr) || !is.finite(sinr)
  m11 <- cosy * cosp
  m12 <- cosy * sinp * sinr - siny * cosr
  m13 <- cosy * sinp * cosr + siny * sinr
  m21 <- siny * cosp
  m22 <- siny * sinp * sinr + cosy * cosr
  m23 <- siny * sinp * cosr - cosy * sinr
  m31 <- -sinp
  m32 <- cosp * sinr
  m33 <- cosp * cosr
  matrices <- lapply(seq_len(n), function(i) {
    if (is_na[i]) {
      return(matrix(NA_real_, ncol = 4, nrow = 4))
    }
    matrix(c(
      m11, m12, m13, 0,
      m21, m22, m23, 0,
      m31, m32, m33, 0,
        0,   0,   0, 1
    ), ncol = 4, nrow = 4)
  })
  new_affine_transformation3(create_transform_3_matrix(matrices))
}

new_affine_transformation2 <- function(x) {
  x <- list(x)
  class(x) <- c("euclid_affine_transformation2", "euclid_affine_transformation")
  x
}
new_affine_transformation3 <- function(x) {
  x <- list(x)
  class(x) <- c("euclid_affine_transformation3", "euclid_affine_transformation")
  x
}

new_affine_transformation_empty <- function(dim) {
  if (dim == 2) {
    new_affine_transformation2(create_transform_2_identity(0L))
  } else {
    new_affine_transformation3(create_transform_3_identity(0L))
  }
}
