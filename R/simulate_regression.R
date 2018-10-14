#' Simulates a 3D Spherical Regression.
#'
#' Returns the response points corresponding to the specified explanatory
#' points, given a model for local rotations
#' and an error term sampler.
#'
#' Let \eqn{E} be
#' the \emph{m}-by-3 matrix of explanatory points.
#' This function will return
#' an \emph{m}-by-\emph{3} matrix whose \eqn{i}-th row is obtained by
#' transposition of the following expression:
#'
#' \deqn{exp(\Phi(\epsilon(x))) exp(\Phi(s(x))) x}
#'
#' where \eqn{x} is the transpose of the \eqn{i}-th row of
#' \eqn{E}. Terms \eqn{\epsilon(x)} and \eqn{s(x)} are obtained by
#' evaluating at \eqn{x} functions \code{local_error_sampler} and
#' \code{local_rotation_composer}, respectively, while
#' matrix \eqn{\Phi(c)}, for a 3-length numeric vector \eqn{c}, is
#' the skew symmetric matrix having its independent components
#' represented by the entries of \eqn{c} (for a thorough discussion,
#' see function
#' \code{\link{get_skew_symmetric_matrix}}).
#'
#' Functions \code{local_error_sampler} and \code{local_rotation_composer}
#' must be prototyped as having one argument, \code{point},
#' representing the Cartesian coordinates of a point on a 3D sphere,
#' and returning a non \code{NULL} numerical object having length
#' equal to \code{3}.
#'
#' @param explanatory_points An \emph{m}-by-3 matrix whose rows contain
#' the Cartesian coordinates of the points at which the regression
#' will be simulated.
#' @param local_rotation_composer A function that returns a
#' 3-length numeric vector representing the independent components of a
#' skew symmetric matrix local to an explanatory point, given its
#' Cartesian coordinates.
#' @param local_error_sampler A function that returns a 3-length numeric vector
#' representing a sampled error term local to an explanatory point,
#' given its Cartesian coordinates.
#' @return An \emph{m}-by-\emph{3} matrix whose rows contain
#' the Cartesian coordinates of the response points corresponding
#' to the explanatory points.
#' @export
#' @family Regression functions
#' @example examples/example_simulate_regression.R
simulate_regression <- function(explanatory_points,
                                local_rotation_composer,
                                local_error_sampler) {
  # Input validation ---

  parameter_validators$explanatory_points$validate(explanatory_points, -4)
  fnc_typed_parameter_validators$local_rotation_composer$validate(
    local_rotation_composer, -4)
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
    local_rotation <- local_rotation_generator(local_rotation_composer(point = x))
    response_points[i, ] <- local_error %*% local_rotation %*% x
  }
  response_points

  #----
}