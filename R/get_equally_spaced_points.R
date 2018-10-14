#' Generates Equally Spaced Points On A 3D Sphere.
#'
#' Generates points approximately equally spaced on a 3D sphere.
#'
#' @param number_of_points A scalar, positive integer
#' representing the number of points to get.
#'
#' @return A \code{number_of_points}-by-3 matrix whose rows
#' contain the Cartesian coordinates
#' of the equally spaced points.
#' @export
#' @family Regression functions
#' @example examples/example_get_equally_spaced_points.R
get_equally_spaced_points <- function(number_of_points) {

  # Input validation ---

  parameter_validators$number_of_points$validate(
    number_of_points, -4)

  # Implementation ---

  equally_spaced_points <- NULL

  inc <- pi * (3 - sqrt(5))
  off <- 2.0 / number_of_points

  for (k in 0:(number_of_points - 1)) {
    y <- k * off - 1 + (off / 2)
    r <- sqrt(1 - y * y)
    phi <- k * inc
    x <- cos(phi) * r
    z <- sin(phi) * r
    equally_spaced_points <- rbind(equally_spaced_points, c(x, y, z))
  }

  equally_spaced_points
}