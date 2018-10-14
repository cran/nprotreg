#' Weights the Specified Explanatory Points in a 3D Spherical Regression.
#'
#' Returns the weights assigned to the specified explanatory points
#' for each evaluation point under study, given a concentration parameter.
#'
#' Let \eqn{X} be the \emph{m}-by-3 matrix of explanatory points, and \eqn{E}
#' the \emph{n}-by-3 matrix of evaluation points, and \eqn{\kappa} the
#' concentration parameter. This function will return
#' an \emph{m}-by-\emph{n} matrix whose \eqn{(i,j)} entry is defined as
#' follows:
#'
#' \deqn{exp(\kappa (s(i,j) - 1))}
#'
#' where \eqn{s(i,j)} is the scalar product of the \eqn{i}-th row of
#' \eqn{X} and the \eqn{j}-th row of \eqn{E}.
#'
#' @param evaluation_points An \emph{n}-by-3 matrix whose rows contain
#' the Cartesian coordinates of the points on which the regression
#' will be estimated.
#' @param explanatory_points An \emph{m}-by-3 matrix whose rows contain
#' the Cartesian coordinates of the explanatory points used to
#' calculate the regression estimators.
#' @param concentration A non negative scalar whose reciprocal value
#' is proportional to the bandwidth applied while estimating
#' a spherical regression model.
#' @return An \emph{m}-by-\emph{n} matrix whose \emph{j}-th column contains
#' the weights assigned to the explanatory points while analyzing the
#' \emph{j}-th evaluation point.
#' @export
#' @family Regression functions
#' @example examples/example_weight_explanatory_points.R
weight_explanatory_points <- function(evaluation_points,
                                      explanatory_points,
                                      concentration) {
  # Input validation ---

  parameter_validators$evaluation_points$validate(evaluation_points, -4)
  parameter_validators$explanatory_points$validate(explanatory_points, -4)
  parameter_validators$concentration$validate(concentration, -4)

  # Implementation ---

  t(exp(concentration * (evaluation_points %*% t(explanatory_points) - 1)))

  #----
}