#include "cgal_types.h"
#include <cpp11/protect.hpp>
#include <vector>

template<typename T>
inline T set_definition_impl(const T& geometry, int which, int element, const Exact_number& value) {
  return geometry;
}

template<typename T>
inline T set_definition_all_impl(const T& geometry, int which, const std::vector<Exact_number>& values) {
  return set_definition_impl(geometry, which, 0, values[0]);
}

template<typename T>
inline std::vector<Point_2> base_set_def_all_2(const T& geometry, int which, const std::vector<Exact_number>& values) {
  std::vector<Point_2> current;
  for (size_t i = 0; i < values.size(); ++i) {
    switch(which) {
    case 0:
      current.emplace_back(values[i].base(), geometry[i].y());
      break;
    case 1:
      current.emplace_back(geometry[i].x(), values[i].base());
      break;
    }
  }
  return current;
}

template<typename T>
inline std::vector<Point_2> base_set_def_one_2(const T& geometry, size_t length, int which, size_t element, const Exact_number& value) {
  std::vector<Point_2> current;
  for (size_t i = 0; i < length; ++i) {
    if (i != element) {
      current.push_back(geometry[i]);
      continue;
    }
    switch(which) {
    case 0:
      current.emplace_back(value.base(), geometry[i].y());
      break;
    case 1:
      current.emplace_back(geometry[i].x(), value.base());
      break;
    }
  }
  return current;
}

template<typename T>
inline std::vector<Point_3> base_set_def_all_3(const T& geometry, int which, const std::vector<Exact_number>& values) {
  std::vector<Point_3> current;
  for (size_t i = 0; i < values.size(); ++i) {
    switch(which) {
    case 0:
      current.emplace_back(values[i].base(), geometry[i].y(), geometry[i].z());
      break;
    case 1:
      current.emplace_back(geometry[i].x(), values[i].base(), geometry[i].z());
      break;
    case 2:
      current.emplace_back(geometry[i].x(), geometry[i].y(), values[i].base());
      break;
    }
  }
  return current;
}

template<typename T>
inline std::vector<Point_3> base_set_def_one_3(const T& geometry, size_t length, int which, size_t element, const Exact_number& value) {
  std::vector<Point_3> current;
  for (size_t i = 0; i < length; ++i) {
    if (i != element) {
      current.push_back(geometry[i]);
      continue;
    }
    switch(which) {
    case 0:
      current.emplace_back(value.base(), geometry[i].y(), geometry[i].z());
      break;
    case 1:
      current.emplace_back(geometry[i].x(), value.base(), geometry[i].z());
      break;
    case 2:
      current.emplace_back(geometry[i].x(), geometry[i].y(), value.base());
      break;
    }
  }
  return current;
}

template<>
inline Circle_2 set_definition_impl(const Circle_2& geometry, int which, int element, const Exact_number& value) {
 switch(which) {
  case 0: return Circle_2(Point_2(value.base(), geometry.center().y()), geometry.squared_radius());
  case 1: return Circle_2(Point_2(geometry.center().x(), value.base()), geometry.squared_radius());
  case 2: return Circle_2(geometry.center(), value.base());
  }
  return geometry;
}

template<>
inline Circle_3 set_definition_impl(const Circle_3& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Circle_3(Point_3(value.base(), geometry.center().y(), geometry.center().z()), geometry.squared_radius(), geometry.supporting_plane().orthogonal_vector());
  case 1: return Circle_3(Point_3(geometry.center().x(), value.base(), geometry.center().z()), geometry.squared_radius(), geometry.supporting_plane().orthogonal_vector());
  case 2: return Circle_3(Point_3(geometry.center().x(), geometry.center().y(), value.base()), geometry.squared_radius(), geometry.supporting_plane().orthogonal_vector());
  case 3: return Circle_3(geometry.center(), value.base(), geometry.supporting_plane());
  case 4: return Circle_3(geometry.center(), geometry.squared_radius(), Vector_3(value.base(), geometry.supporting_plane().orthogonal_direction().dy(), geometry.supporting_plane().orthogonal_direction().dz()));
  case 5: return Circle_3(geometry.center(), geometry.squared_radius(), Vector_3(geometry.supporting_plane().orthogonal_direction().dx(), value.base(), geometry.supporting_plane().orthogonal_direction().dz()));
  case 6: return Circle_3(geometry.center(), geometry.squared_radius(), Vector_3(geometry.supporting_plane().orthogonal_direction().dx(), geometry.supporting_plane().orthogonal_direction().dy(), value.base()));
  }
  return geometry;
}

template<>
inline Direction_2 set_definition_impl(const Direction_2& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Direction_2(value.base(), geometry.dy());
  case 1: return Direction_2(geometry.dx(), value.base());
  }
  return geometry;
}

template<>
inline Direction_3 set_definition_impl(const Direction_3& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Direction_3(value.base(), geometry.dy(), geometry.dz());
  case 1: return Direction_3(geometry.dx(), value.base(), geometry.dz());
  case 2: return Direction_3(geometry.dx(), geometry.dy(), value.base());
  }
  return geometry;
}

template<>
inline Iso_cuboid set_definition_impl(const Iso_cuboid& geometry, int which, int element, const Exact_number& value) {
  cpp11::stop("iso_cube geometries can't be modied one definition at a time");
}

template<>
inline Iso_rectangle set_definition_impl(const Iso_rectangle& geometry, int which, int element, const Exact_number& value) {
  cpp11::stop("iso_rect geometries can't be modied one definition at a time");
}

template<>
inline Line_2 set_definition_impl(const Line_2& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Line_2(value.base(), geometry.b(), geometry.c());
  case 1: return Line_2(geometry.a(), value.base(), geometry.c());
  case 2: return Line_2(geometry.a(), geometry.b(), value.base());
  }
  return geometry;
}

template<>
inline Line_3 set_definition_impl(const Line_3& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Line_3(Point_3(value.base(), geometry.point(0.0).y(), geometry.point(0.0).z()), geometry.direction());
  case 1: return Line_3(Point_3(geometry.point(0.0).x(), value.base(), geometry.point(0.0).z()), geometry.direction());
  case 2: return Line_3(Point_3(geometry.point(0.0).x(), geometry.point(0.0).y(), value.base()), geometry.direction());
  case 3: return Line_3(geometry.point(0.0), Direction_3(value.base(), geometry.direction().dy(), geometry.direction().dz()));
  case 4: return Line_3(geometry.point(0.0), Direction_3(geometry.direction().dx(), value.base(), geometry.direction().dz()));
  case 5: return Line_3(geometry.point(0.0), Direction_3(geometry.direction().dx(), geometry.direction().dy(), value.base()));
  }
  return geometry;
}

template<>
inline Plane set_definition_impl(const Plane& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Plane(value.base(), geometry.b(), geometry.c(), geometry.d());
  case 1: return Plane(geometry.a(), value.base(), geometry.c(), geometry.d());
  case 2: return Plane(geometry.a(), geometry.b(), value.base(), geometry.d());
  case 3: return Plane(geometry.a(), geometry.b(), geometry.c(), value.base());
  }
  return geometry;
}

template<>
inline Weighted_point_2 set_definition_impl(const Weighted_point_2& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Weighted_point_2(Point_2(value.base(), geometry.y()), geometry.weight());
  case 1: return Weighted_point_2(Point_2(geometry.x(), value.base()), geometry.weight());
  case 2: return Weighted_point_2(geometry.point(), value.base());
  }
  return geometry;
}

template<>
inline Weighted_point_3 set_definition_impl(const Weighted_point_3& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Weighted_point_3(Point_3(value.base(), geometry.y(), geometry.z()), geometry.weight());
  case 1: return Weighted_point_3(Point_3(geometry.x(), value.base(), geometry.z()), geometry.weight());
  case 2: return Weighted_point_3(Point_3(geometry.x(), geometry.y(), value.base()), geometry.weight());
  case 3: return Weighted_point_3(geometry.point(), value.base());
  }
  return geometry;
}

template<>
inline Point_2 set_definition_impl(const Point_2& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Point_2(value.base(), geometry.y());
  case 1: return Point_2(geometry.x(), value.base());
  }
  return geometry;
}

template<>
inline Point_3 set_definition_impl(const Point_3& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Point_3(value.base(), geometry.y(), geometry.z());
  case 1: return Point_3(geometry.x(), value.base(), geometry.z());
  case 2: return Point_3(geometry.x(), geometry.y(), value.base());
  }
  return geometry;
}

template<>
inline Ray_2 set_definition_impl(const Ray_2& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Ray_2(Point_2(value.base(), geometry.source().y()), geometry.direction());
  case 1: return Ray_2(Point_2(geometry.source().x(), value.base()), geometry.direction());
  case 2: return Ray_2(geometry.source(), Direction_2(value.base(), geometry.direction().dy()));
  case 3: return Ray_2(geometry.source(), Direction_2(geometry.direction().dx(), value.base()));
  }
  return geometry;
}

template<>
inline Ray_3 set_definition_impl(const Ray_3& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Ray_3(Point_3(value.base(), geometry.source().y(), geometry.source().z()), geometry.direction());
  case 1: return Ray_3(Point_3(geometry.source().x(), value.base(), geometry.source().z()), geometry.direction());
  case 2: return Ray_3(Point_3(geometry.source().x(), geometry.source().y(), value.base()), geometry.direction());
  case 3: return Ray_3(geometry.source(), Direction_3(value.base(), geometry.direction().dy(), geometry.direction().dz()));
  case 4: return Ray_3(geometry.source(), Direction_3(geometry.direction().dx(), value.base(), geometry.direction().dz()));
  case 5: return Ray_3(geometry.source(), Direction_3(geometry.direction().dx(), geometry.direction().dy(), value.base()));
  }
  return geometry;
}

template<>
inline Segment_2 set_definition_impl(const Segment_2& geometry, int which, int element, const Exact_number& value) {
  std::vector<Point_2> current = base_set_def_one_2(geometry, 2, which, element, value);
  return Segment_2(current[0], current[1]);
}

template<>
inline Segment_3 set_definition_impl(const Segment_3& geometry, int which, int element, const Exact_number& value) {
  std::vector<Point_3> current = base_set_def_one_3(geometry, 2, which, element, value);
  return Segment_3(current[0], current[1]);
}

template<>
inline Segment_2 set_definition_all_impl(const Segment_2& geometry, int which, const std::vector<Exact_number>& values) {
  std::vector<Point_2> current = base_set_def_all_2(geometry, which, values);
  return Segment_2(current[0], current[1]);
}

template<>
inline Segment_3 set_definition_all_impl(const Segment_3& geometry, int which, const std::vector<Exact_number>& values) {
  std::vector<Point_3> current = base_set_def_all_3(geometry, which, values);
  return Segment_3(current[0], current[1]);
}

template<>
inline Sphere set_definition_impl(const Sphere& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Sphere(Point_3(value.base(), geometry.center().y(), geometry.center().z()), geometry.squared_radius());
  case 1: return Sphere(Point_3(geometry.center().x(), value.base(), geometry.center().z()), geometry.squared_radius());
  case 2: return Sphere(Point_3(geometry.center().x(), geometry.center().y(), value.base()), geometry.squared_radius());
  case 3: return Sphere(geometry.center(), value.base());
  }
  return geometry;
}

template<>
inline Tetrahedron set_definition_impl(const Tetrahedron& geometry, int which, int element, const Exact_number& value) {
  std::vector<Point_3> current = base_set_def_one_3(geometry, 4, which, element, value);
  return Tetrahedron(current[0], current[1], current[2], current[3]);
}

template<>
inline Tetrahedron set_definition_all_impl(const Tetrahedron& geometry, int which, const std::vector<Exact_number>& values) {
  std::vector<Point_3> current = base_set_def_all_3(geometry, which, values);
  return Tetrahedron(current[0], current[1], current[2], current[3]);
}

template<>
inline Triangle_2 set_definition_impl(const Triangle_2& geometry, int which, int element, const Exact_number& value) {
  std::vector<Point_2> current = base_set_def_one_2(geometry, 3, which, element, value);
  return Triangle_2(current[0], current[1], current[2]);
}

template<>
inline Triangle_3 set_definition_impl(const Triangle_3& geometry, int which, int element, const Exact_number& value) {
  std::vector<Point_3> current = base_set_def_one_3(geometry, 3, which, element, value);
  return Triangle_3(current[0], current[1], current[2]);
}

template<>
inline Triangle_2 set_definition_all_impl(const Triangle_2& geometry, int which, const std::vector<Exact_number>& values) {
  std::vector<Point_2> current = base_set_def_all_2(geometry, which, values);
  return Triangle_2(current[0], current[1], current[2]);
}

template<>
inline Triangle_3 set_definition_all_impl(const Triangle_3& geometry, int which, const std::vector<Exact_number>& values) {
  std::vector<Point_3> current = base_set_def_all_3(geometry, which, values);
  return Triangle_3(current[0], current[1], current[2]);
}

template<>
inline Vector_2 set_definition_impl(const Vector_2& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Vector_2(value.base(), geometry.y());
  case 1: return Vector_2(geometry.x(), value.base());
  }
  return geometry;
}

template<>
inline Vector_3 set_definition_impl(const Vector_3& geometry, int which, int element, const Exact_number& value) {
  switch(which) {
  case 0: return Vector_3(value.base(), geometry.y(), geometry.z());
  case 1: return Vector_3(geometry.x(), value.base(), geometry.z());
  case 2: return Vector_3(geometry.x(), geometry.y(), value.base());
  }
  return geometry;
}
