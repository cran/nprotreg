% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weight_explanatory_points.R
\name{weight_explanatory_points}
\alias{weight_explanatory_points}
\title{Weights the Specified Explanatory Points in a 3D Spherical Regression.}
\usage{
weight_explanatory_points(evaluation_points, explanatory_points, concentration)
}
\arguments{
\item{evaluation_points}{An \emph{n}-by-3 matrix whose rows contain
the Cartesian coordinates of the points on which the regression
will be estimated.}

\item{explanatory_points}{An \emph{m}-by-3 matrix whose rows contain
the Cartesian coordinates of the explanatory points used to
calculate the regression estimators.}

\item{concentration}{A non negative scalar whose reciprocal value
is proportional to the bandwidth applied while estimating
a spherical regression model.}
}
\value{
An \emph{m}-by-\emph{n} matrix whose \emph{j}-th column contains
the weights assigned to the explanatory points while analyzing the
\emph{j}-th evaluation point.
}
\description{
Returns the weights assigned to the specified explanatory points
for each evaluation point under study, given a concentration parameter.
}
\details{
Let \eqn{X} be the \emph{m}-by-3 matrix of explanatory points, and \eqn{E}
the \emph{n}-by-3 matrix of evaluation points, and \eqn{\kappa} the
concentration parameter. This function will return
an \emph{m}-by-\emph{n} matrix whose \eqn{(i,j)} entry is defined as
follows:

\deqn{exp(\kappa (s(i,j) - 1))}

where \eqn{s(i,j)} is the scalar product of the \eqn{i}-th row of
\eqn{X} and the \eqn{j}-th row of \eqn{E}.
}
\examples{
library(nprotreg)

# Define a matrix of evaluation points.

north_pole <- cbind(0, 0, 1)
south_pole <- cbind(0, 0, -1)
evaluation_points <- rbind(north_pole, south_pole)

# Define a matrix of explanatory points

explanatory_points <- rbind(
  cbind(.5, 0, .8660254),
  cbind(-.5, 0, .8660254),
  cbind(1, 0, 0),
  cbind(0, 1, 0),
  cbind(-1, 0, 0),
  cbind(0, -1, 0),
  cbind(.5, 0, -.8660254),
  cbind(-.5, 0, -.8660254)
)

# Define a value for the concentration parameter.

concentration <- 1.0

# Get the corresponding 8-by-2 matrix of weights.
# Columns corresponds to evaluation points,
# rows to explanatory ones.

weights <- weight_explanatory_points(evaluation_points,
                                     explanatory_points,
                                     concentration)

# Get the weights assigned to the explanatory points
# while analyzing the second evaluation point.

cat("Weights assigned while analyzing the second evaluation point: \n")
cat(weights[, 2], "\n")
}
\seealso{
Other Regression functions: 
\code{\link{cross_validate_concentration}()},
\code{\link{fit_regression}()},
\code{\link{get_equally_spaced_points}()},
\code{\link{get_skew_symmetric_matrix}()},
\code{\link{simulate_regression}()},
\code{\link{simulate_rigid_regression}()}
}
\concept{Regression functions}
