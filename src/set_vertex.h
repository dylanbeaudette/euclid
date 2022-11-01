#include "cgal_types.h"
#include <cpp11/protect.hpp>

template<typename T, typename U>
inline T set_vertex_impl(const T& geometry, int which, const U& value) {
  return geometry;
}

template<>
inline Circle_2 set_vertex_impl(const Circle_2& geometry, int which, const Point_2& value) {
  return Circle_2(value, geometry.squared_radius());
}

template<>
inline Circle_3 set_vertex_impl(const Circle_3& geometry, int which, const Point_3& value) {
  return Circle_3(value, geometry.squared_radius(), geometry.supporting_plane().orthogonal_vector());
}

template<>
inline Line_2 set_vertex_impl(const Line_2& geometry, int which, const Point_2& value) {
  return Line_2(value, geometry.direction());
}

template<>
inline Line_3 set_vertex_impl(const Line_3& geometry, int which, const Point_3& value) {
  return Line_3(value, geometry.direction());
}

template<>
inline Plane set_vertex_impl(const Plane& geometry, int which, const Point_3& value) {
  return Plane(value, geometry.orthogonal_vector());
}

template<>
inline Point_2 set_vertex_impl(const Point_2& geometry, int which, const Point_2& value) {
  return value;
}

template<>
inline Point_3 set_vertex_impl(const Point_3& geometry, int which, const Point_3& value) {
  return value;
}

template<>
inline Weighted_point_2 set_vertex_impl(const Weighted_point_2& geometry, int which, const Point_2& value) {
  return Weighted_point_2(value, geometry.weight());
}

template<>
inline Weighted_point_3 set_vertex_impl(const Weighted_point_3& geometry, int which, const Point_3& value) {
  return Weighted_point_3(value, geometry.weight());
}

template<>
inline Ray_2 set_vertex_impl(const Ray_2& geometry, int which, const Point_2& value) {
  return Ray_2(value, geometry.direction());
}

template<>
inline Ray_3 set_vertex_impl(const Ray_3& geometry, int which, const Point_3& value) {
  return Ray_3(value, geometry.direction());
}

template<>
inline Segment_2 set_vertex_impl(const Segment_2& geometry, int which, const Point_2& value) {
  std::vector<Point_2> current = {geometry.source(), geometry.target()};
  current[which] = value;
  return Segment_2(current[0], current[1]);
}

template<>
inline Segment_3 set_vertex_impl(const Segment_3& geometry, int which, const Point_3& value) {
  std::vector<Point_3> current = {geometry.source(), geometry.target()};
  current[which] = value;
  return Segment_3(current[0], current[1]);
}

template<>
inline Sphere set_vertex_impl(const Sphere& geometry, int which, const Point_3& value) {
  return Sphere(value, geometry.squared_radius());
}

template<>
inline Tetrahedron set_vertex_impl(const Tetrahedron& geometry, int which, const Point_3& value) {
  std::vector<Point_3> current = {geometry.vertex(0), geometry.vertex(1), geometry.vertex(2), geometry.vertex(3)};
  current[which] = value;
  return Tetrahedron(current[0], current[1], current[2], current[3]);
}

template<>
inline Triangle_2 set_vertex_impl(const Triangle_2& geometry, int which, const Point_2& value) {
  std::vector<Point_2> current = {geometry.vertex(0), geometry.vertex(1), geometry.vertex(2)};
  current[which] = value;
  return Triangle_2(current[0], current[1], current[2]);
}

template<>
inline Triangle_3 set_vertex_impl(const Triangle_3& geometry, int which, const Point_3& value) {
  std::vector<Point_3> current = {geometry.vertex(0), geometry.vertex(1), geometry.vertex(2)};
  current[which] = value;
  return Triangle_3(current[0], current[1], current[2]);
}
