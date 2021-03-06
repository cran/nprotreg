% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_cartesian_to_spherical.R
\name{convert_cartesian_to_spherical}
\alias{convert_cartesian_to_spherical}
\title{Converts Cartesian to Spherical Coordinates.}
\usage{
convert_cartesian_to_spherical(cartesian_coords)
}
\arguments{
\item{cartesian_coords}{A matrix whose rows contain the Cartesian coordinates
of the specified points.}
}
\value{
A matrix of rows containing the longitude and
latitude of specific points on a 3-dimensional sphere.
}
\description{
The Cartesian coordinates of points on a 3-dimensional
sphere with unit radius and center at the origin are
converted to the
equivalent longitude and latitude coordinates, measured in radians.
}
\examples{
library(nprotreg)

# Define the Cartesian coordinates of the North and South Poles.

north_pole <- cbind(0, 0, 1)
south_pole <- cbind(0, 0, -1)
cartesian_coords <- rbind(north_pole, south_pole)

# Get the corresponding Spherical coordinates.

spherical_coords <- convert_cartesian_to_spherical(cartesian_coords)
}
\seealso{
\url{http://mathworld.wolfram.com/SphericalCoordinates.html}.

Other Conversion functions: 
\code{\link{convert_spherical_to_cartesian}()}
}
\concept{Conversion functions}
