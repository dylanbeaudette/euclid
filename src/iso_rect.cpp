#include "iso_rect.h"
#include "point.h"
#include "exact_numeric.h"
#include "bbox.h"

[[cpp11::register]]
iso_rect_p create_iso_rect_empty() {
  std::vector<Iso_rectangle> vec;
  iso_rect *result(new iso_rect(vec));
  return {result};
}

[[cpp11::register]]
iso_rect_p create_iso_rect_pq(point2_p p, point2_p q) {
  if (p.get() == nullptr || q.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Iso_rectangle> vec;
  vec.reserve(p->size());
  for (size_t i = 0; i < p->size(); ++i) {
    if (!(*p)[i] || !(*q)[i]) {
      vec.push_back(Iso_rectangle::NA_value());
      continue;
    }
    vec.emplace_back((*p)[i], (*q)[i]);
  }
  iso_rect *result(new iso_rect(vec));

  return {result};
}

[[cpp11::register]]
iso_rect_p create_iso_rect_minmax(exact_numeric_p minx, exact_numeric_p miny, exact_numeric_p maxx, exact_numeric_p maxy) {
  if (minx.get() == nullptr || miny.get() == nullptr || maxx.get() == nullptr || maxy.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Iso_rectangle> vec;
  vec.reserve(minx->size());
  for (size_t i = 0; i < minx->size(); ++i) {
    if (!(*minx)[i] || !(*miny)[i] || !(*maxx)[i] || !(*maxy)[i]) {
      vec.push_back(Iso_rectangle::NA_value());
      continue;
    }
    vec.emplace_back((*minx)[i], (*miny)[i], (*maxx)[i], (*maxy)[i]);
  }
  iso_rect *result(new iso_rect(vec));

  return {result};
}

[[cpp11::register]]
iso_rect_p create_iso_rect_bbox(bbox2_p bbox) {
  if (bbox.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Iso_rectangle> vec;
  vec.reserve(bbox->size());
  for (size_t i = 0; i < bbox->size(); ++i) {
    if (!(*bbox)[i]) {
      vec.push_back(Iso_rectangle::NA_value());
      continue;
    }
    vec.emplace_back((*bbox)[i]);
  }
  iso_rect *result(new iso_rect(vec));

  return {result};
}
