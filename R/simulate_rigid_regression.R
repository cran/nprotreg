#' Simulates a Rigid 3D Spherical Regression.
#'
#' Returns the response points corresponding to the specified explanatory
#' points, given a rigid rotation model
#' and an error term sampler.
#'
#' Let \eqn{E} be
#' the \emph{m}-by-3 matrix of explanatory points.
#' This function will return
#' an \emph{m}-by-\emph{3} matrix whose \eqn{i}-th row is obtained by
#' transposition of the following expression:
#'
#' \deqn{exp(\Phi(\epsilon(x))) R x}
#'
#' where \eqn{x} is the transpose of the \eqn{i}-th row of
#' \eqn{E} and \eqn{R} is \code{rotation_matrix}.
#' Term \eqn{\epsilon(x)} is obtained by
#' evaluating at \eqn{x} function \code{local_error_sampler}, while
#' matrix \eqn{\Phi(c)}, for a 3-length numeric vector \eqn{c}, is
#' the skew symmetric matrix having its independent components
#' represented by the entries of \eqn{c} (for a thorough discussion,
#' see function
#' \code{\link{get_skew_symmetric_matrix}}).
#'
#' Function \code{local_error_sampler}
#' must be prototyped as having one argument, \code{point},
#' representing the Cartesian coordinates of a point on a 3D sphere,
#' and returning a non \code{NULL} numerical object having length
#' equal to \code{3}.
#'
#' @param explanatory_points An \emph{m}-by-3 matrix whose rows contain
#' the Cartesian coordinates of the points at which the regression
#' will be simulated.
#' @param rotation_matrix A 3-by-3 rotation matrix.
#' @param local_error_sampler A function that returns a 3-length numeric vector
#' representing a sampled error term local to an explanatory point,
#' given its Cartesian coordinates.
#' @return An \emph{m}-by-\emph{3} matrix whose rows contain
#' the Cartesian coordinates of the response points corresponding
#' to the explanatory points.
#' @export
#' @family Regression functions
#' @example examples/example_simulate_rigid_regression.R
simulate_rigid_regression <- function(explanatory_points,
                                      rotation_matrix,
                                      local_error_sampler) {
  # Input validation ---

  parameter_validators$explanatory_points$validate(explanatory_points, -4)
  parameter_validators$rotation_matrix$validate(
    rotation_matrix, -4)
  fnc_typed_parameter_validators$local_error_sampler$validate(
    local_error_sampler, -4)

  # Implementation ---

  local_rotation_generator <- function(
    independent_components) {
    skew_symmetric_matrix <- get_skew_symmetric_matrix(
      independent_components
    )
    expm(skew_symmetric_matrix)
  }

  number_of_points <- nrow(explanatory_points)
  response_points <- matrix(nrow = number_of_points, ncol = 3)
  for (i in 1:number_of_points) {
    x <- explanatory_points[i, ]
    local_error <- local_rotation_generator(local_error_sampler(point = x))
    response_points[i, ] <- local_error %*% rotation_matrix %*% x
  }
  response_points

  #----
}