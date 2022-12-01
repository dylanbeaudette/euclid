#include "geometry_vector.h"

[[cpp11::register]]
geometry_vector_base_p geometry_project_to_line(geometry_vector_base_p geometries, geometry_vector_base_p lines) {
  if (geometries.get() == nullptr || lines.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->project_to_line(*lines);
}

[[cpp11::register]]
geometry_vector_base_p geometry_project_to_plane(geometry_vector_base_p geometries, geometry_vector_base_p planes) {
  if (geometries.get() == nullptr || planes.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->project_to_plane(*planes);
}

[[cpp11::register]]
geometry_vector_base_p geometry_map_to_plane(geometry_vector_base_p geometries, geometry_vector_base_p planes) {
  if (geometries.get() == nullptr || planes.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->map_to_plane(*planes);
}

[[cpp11::register]]
geometry_vector_base_p geometry_normal(geometry_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->normal();
}
