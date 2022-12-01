#include "sphere.h"
#include "point.h"
#include "circle.h"

[[cpp11::register]]
sphere_p create_sphere_empty() {
  std::vector<Sphere> vec;
  sphere *result(new sphere(vec));
  return {result};
}

[[cpp11::register]]
sphere_p create_sphere_center_radius(point3_p center, exact_numeric_p r2) {
  if (center.get() == nullptr || r2.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Sphere> vec;
  vec.reserve(center->size());
  for (size_t i = 0; i < center->size(); ++i) {
    if (!(*center)[i] || !(*r2)[i] || (*r2)[i] < 0.0) {
      vec.push_back(Sphere::NA_value());
      continue;
    }
    vec.emplace_back((*center)[i], (*r2)[i]);
  }
  sphere *result(new sphere(vec));

  return {result};
}

[[cpp11::register]]
sphere_p create_sphere_4_point(point3_p p, point3_p q, point3_p r, point3_p s) {
  if (p.get() == nullptr || q.get() == nullptr || r.get() == nullptr || s.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Sphere> vec;
  vec.reserve(p->size());
  for (size_t i = 0; i < p->size(); ++i) {
    if (!(*p)[i] || !(*q)[i] || !(*r)[i] || !(*s)[i]) {
      vec.push_back(Sphere::NA_value());
      continue;
    }
    if (CGAL::coplanar((*p)[i], (*q)[i], (*r)[i], (*s)[i])) {
      cpp11::warning("sphere cannot be constructed from 4 coplanar points");
      vec.push_back(Sphere::NA_value());
      continue;
    }
    vec.emplace_back((*p)[i], (*q)[i], (*r)[i], (*s)[i]);
  }
  sphere *result(new sphere(vec));

  return {result};
}

[[cpp11::register]]
sphere_p create_sphere_3_point(point3_p p, point3_p q, point3_p r) {
  if (p.get() == nullptr || q.get() == nullptr || r.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Sphere> vec;
  vec.reserve(p->size());
  for (size_t i = 0; i < p->size(); ++i) {
    if (!(*p)[i] || !(*q)[i] || !(*r)[i]) {
      vec.push_back(Sphere::NA_value());
      continue;
    }
    vec.emplace_back((*p)[i], (*q)[i], (*r)[i]);
  }
  sphere *result(new sphere(vec));

  return {result};
}

[[cpp11::register]]
sphere_p create_sphere_2_point(point3_p p, point3_p q) {
  if (p.get() == nullptr || q.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Sphere> vec;
  vec.reserve(p->size());
  for (size_t i = 0; i < p->size(); ++i) {
    if (!(*p)[i] || !(*q)[i]) {
      vec.push_back(Sphere::NA_value());
      continue;
    }
    vec.emplace_back((*p)[i], (*q)[i]);
  }
  sphere *result(new sphere(vec));

  return {result};
}

[[cpp11::register]]
sphere_p create_sphere_circle(circle3_p circ) {
  if (circ.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Sphere> vec;
  vec.reserve(circ->size());
  for (size_t i = 0; i < circ->size(); ++i) {
    if (!(*circ)[i]) {
      vec.push_back(Sphere::NA_value());
      continue;
    }
    vec.emplace_back((*circ)[i]);
  }
  sphere *result(new sphere(vec));

  return {result};
}
