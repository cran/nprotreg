#' Converts Cartesian to Spherical Coordinates.
#'
#' The Cartesian coordinates of points on a 3-dimensional
#' sphere with unit radius and center at the origin are
#' converted to the
#' equivalent longitude and latitude coordinates, measured in radians.
#'
#' @param cartesian_coords A matrix whose rows contain the Cartesian coordinates
#' of the specified points.
#'
#' @return A matrix of rows containing the longitude and
#' latitude of specific points on a 3-dimensional sphere.
#' @export
#' @family Conversion functions
#' @seealso \url{http://mathworld.wolfram.com/SphericalCoordinates.html}.
#' @example examples/example_convert_cartesian_to_spherical.R
convert_cartesian_to_spherical <- function(cartesian_coords) {

  # Input validation ---

  parameter_validators$cartesian_coords$validate(
    cartesian_coords, -4)

  # Implementation ---

  # input is [x, y, z], output is [longitude, latitude].
  lon <- atan2(cartesian_coords[, 2], cartesian_coords[, 1])
  lat <- pi / 2 - acos(cartesian_coords[, 3])
  cbind(lon, lat)
}