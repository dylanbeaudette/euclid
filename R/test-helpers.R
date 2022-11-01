create_bad_lines <- function(dim = 2) {
  if (dim == 2) {
    l <- line(point(0, 0), vec(c(1, 0), c(1, 0)))
  } else {
    l <- line(point(0, 0, 0), vec(c(1, 0), c(1, 0), c(1, 0)))
  }
  l[c(1, 2, NA)]
}
