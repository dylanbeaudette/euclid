#include "cgal_types.h"
#include <cpp11/protect.hpp>

template<typename T, typename U>
inline U get_edge_impl(const T& geometry, int which) {
  return U::NA_value();
}

template<>
inline Segment_2 get_edge_impl(const Segment_2& geometry, int which) {
  return geometry;
}

template<>
inline Segment_3 get_edge_impl(const Segment_3& geometry, int which) {
  return geometry;
}

template<>
inline Segment_2 get_edge_impl(const Iso_rectangle& geometry, int which) {
  switch (which) {
    case 0: return Segment_2(geometry.vertex(0), geometry.vertex(1));
    case 1: return Segment_2(geometry.vertex(1), geometry.vertex(2));
    case 2: return Segment_2(geometry.vertex(2), geometry.vertex(3));
    case 3: return Segment_2(geometry.vertex(3), geometry.vertex(0));
  }
  return Segment_2::NA_value();
}

template<>
inline Segment_3 get_edge_impl(const Iso_cuboid& geometry, int which) {
  switch (which) {
  case 0: return Segment_3(geometry.vertex(0), geometry.vertex(1));
  case 1: return Segment_3(geometry.vertex(1), geometry.vertex(2));
  case 2: return Segment_3(geometry.vertex(2), geometry.vertex(3));
  case 3: return Segment_3(geometry.vertex(3), geometry.vertex(0));
  case 4: return Segment_3(geometry.vertex(4), geometry.vertex(5));
  case 5: return Segment_3(geometry.vertex(5), geometry.vertex(6));
  case 6: return Segment_3(geometry.vertex(6), geometry.vertex(7));
  case 7: return Segment_3(geometry.vertex(7), geometry.vertex(4));
  case 8: return Segment_3(geometry.vertex(0), geometry.vertex(5));
  case 9: return Segment_3(geometry.vertex(1), geometry.vertex(6));
  case 10: return Segment_3(geometry.vertex(2), geometry.vertex(7));
  case 11: return Segment_3(geometry.vertex(3), geometry.vertex(4));
  }
  return Segment_3::NA_value();
}

template<>
inline Segment_2 get_edge_impl(const Triangle_2& geometry, int which) {
  switch (which) {
  case 0: return Segment_2(geometry.vertex(0), geometry.vertex(1));
  case 1: return Segment_2(geometry.vertex(1), geometry.vertex(2));
  case 2: return Segment_2(geometry.vertex(2), geometry.vertex(0));
  }
  return Segment_2::NA_value();
}

template<>
inline Segment_3 get_edge_impl(const Triangle_3& geometry, int which) {
  switch (which) {
  case 0: return Segment_3(geometry.vertex(0), geometry.vertex(1));
  case 1: return Segment_3(geometry.vertex(1), geometry.vertex(2));
  case 2: return Segment_3(geometry.vertex(2), geometry.vertex(0));
  }
  return Segment_3::NA_value();
}

template<>
inline Segment_3 get_edge_impl(const Tetrahedron& geometry, int which) {
  switch (which) {
  case 0: return Segment_3(geometry.vertex(0), geometry.vertex(1));
  case 1: return Segment_3(geometry.vertex(1), geometry.vertex(2));
  case 2: return Segment_3(geometry.vertex(2), geometry.vertex(0));
  case 3: return Segment_3(geometry.vertex(0), geometry.vertex(4));
  case 4: return Segment_3(geometry.vertex(1), geometry.vertex(4));
  case 5: return Segment_3(geometry.vertex(2), geometry.vertex(4));
  }
  return Segment_3::NA_value();
}
