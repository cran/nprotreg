#' Converts Spherical to Cartesian Coordinates.
#'
#' The longitude and latitude coordinates of points on a 3-dimensional
#' sphere with unit radius and center at the origin
#' are converted to the
#' equivalent Cartesian coordinates.
#'
#' @param spherical_coords A matrix of rows containing the longitude and
#' latitude, measured in radians, of specific points on a 3-dimensional sphere.
#'
#' @return A matrix whose rows contain the Cartesian coordinates
#' of the specified points.
#' @export
#' @family Conversion functions
#' @seealso \url{http://mathworld.wolfram.com/SphericalCoordinates.html}.
#' @example examples/example_convert_spherical_to_cartesian.R
convert_spherical_to_cartesian <- function(spherical_coords) {

  # Input validation ---

  parameter_validators$spherical_coords$validate(
    spherical_coords, -4)

  # Implementation ---

  # input is [longitude, latitude], output [x, y, z].
  lon <- spherical_coords[, 1]
  lat <- spherical_coords[, 2]
  rect_angle <- pi / 2
  s <- sin(rect_angle - lat)
  x <- cos(lon) * s
  y <- sin(lon) * s
  z <- cos(rect_angle - lat)
  cbind(x, y, z)
}