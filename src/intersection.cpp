#include "geometry_vector.h"
#include <cpp11/list.hpp>
#include <cpp11/logicals.hpp>

[[cpp11::register]]
cpp11::writable::list geometry_intersection(geometry_vector_base_p geo1, geometry_vector_base_p geo2) {
  if (geo1.get() == nullptr || geo2.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geo1->intersection(*geo2);
}

[[cpp11::register]]
cpp11::writable::logicals geometry_do_intersect(geometry_vector_base_p geo1, geometry_vector_base_p geo2) {
  if (geo1.get() == nullptr || geo2.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geo1->do_intersect(*geo2);
}
