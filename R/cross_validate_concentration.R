#' Cross-validates The Concentration Parameter In A 3D Spherical Regression.
#'
#' Returns a cross-validated value for the concentration parameter in
#' a 3D regression, relating specific explanatory points to response
#' ones, given a weighting scheme for the observed data set.
#' This function supports the method for sphere-sphere regression
#' proposed by Di Marzio et al. (2018).
#'
#' Function \code{weights_generator} must be prototyped as having the
#' following three arguments:
#' \describe{
#'  \item{\code{evaluation_points}}{a matrix whose \emph{n} rows are the Cartesian coordinates of
#' given evaluation points.}
#'  \item{\code{explanatory_points}}{a matrix whose \emph{m} rows are the Cartesian coordinates of
#' given explanatory points.}
#'  \item{\code{concentration}}{A non negative scalar whose reciprocal value
#' is proportional to the bandwidth applied while estimating
#' a spherical regression model.}
#' }
#' It is also expected that \code{weights_generator} will return
#' a non \code{NULL} numerical \emph{m}-by-\emph{n} matrix whose \emph{j}-th column contains
#' the weights assigned to the explanatory points while analyzing the
#' \emph{j}-th evaluation point.
#'
#' @param concentration_upper_bound A scalar numeric value representing
#' the upper end-point of the interval to be searched for the required minimizer.
#' Defaults to \code{10}.
#' @param explanatory_points An \emph{m}-by-3 matrix whose rows contain
#' the Cartesian coordinates of the explanatory points used to
#' calculate the regression estimators.
#' @param response_points An \emph{m}-by-\emph{3} matrix whose rows contain
#' the Cartesian coordinates of the response points corresponding
#' to the explanatory points.
#' @param weights_generator A function that, given a matrix of \emph{n}
#' evaluation points, returns an \emph{m}-by-\emph{n} matrix whose
#' \emph{j}-th column contains
#' the weights assigned to the explanatory points while analyzing the
#' \emph{j}-th evaluation point. Defaults to \code{\link{weight_explanatory_points}}.
#' @param number_of_expansion_terms The number of terms to be included
#' in the expansion of the matrix exponential applied while
#' approximating a local rotation matrix. Must be \code{1} or \code{2}.
#' Defaults to \code{1}.
#' @param number_of_iterations The number of
#' rotation fitting steps to be executed.
#' At each step, the points estimated during the previous step
#' are exploited as the current explanatory points. Defaults to \code{1}.
#' @param allow_reflections A logical scalar value. If set to \code{TRUE} signals
#' that reflections are allowed. Defaults to \code{FALSE}. It is ignored if
#' \code{number_of_expansion_terms} is \code{2}.
#' @return A list having two components,
#' \code{concentration}, a scalar, numeric value representing the cross-validated
#' concentration for the specified 3D regression, and
#' \code{objective}, the value of the cross-validating objective function at argument \code{concentration}.
#' @export
#' @family Regression functions
#' @example examples/example_cross_validate_concentration.R
#' @references Marco Di Marzio, Agnese Panzera & Charles C. Taylor (2018)
#' Nonparametric rotations for sphere-sphere regression,
#' Journal of the American Statistical Association,
#' <doi:10.1080/01621459.2017.1421542>.
cross_validate_concentration <- function(
  concentration_upper_bound = 10,
  explanatory_points,
  response_points,
  weights_generator = weight_explanatory_points,
  number_of_expansion_terms = 1,
  number_of_iterations = 1,
  allow_reflections = FALSE
) {
  # Input validation ---

  parameter_validators$concentration_upper_bound$validate(concentration_upper_bound, -4)
  parameter_validators$explanatory_points$validate(explanatory_points, -4)

  parameter_validators$explanatory_points$check_minimum_number_of_rows(
    explanatory_points,
    3, -3
  )

  parameter_validators$response_points$validate(response_points, -4)

  parameter_validators$response_points$check_shared_number_of_rows(
    response_points,
    explanatory_points,
    parameter_validators$explanatory_points$name, -3
  )

  fnc_typed_parameter_validators$weights_generator$validate(weights_generator, -4)

  parameter_validators$number_of_expansion_terms$validate(
    number_of_expansion_terms, -4
  )

  parameter_validators$number_of_iterations$validate(
    number_of_iterations, -4
  )

  parameter_validators$allow_reflections$validate(allow_reflections, -4)

  # Implementation ---

  local_rotation_modeler <- switch(
    number_of_expansion_terms,
    one_term_local_rotation_modeler,
    two_term_local_rotation_modeler
  )
  
  optimization_info <- optimize(
    f = cross_validating_objective,
    interval = c(0, concentration_upper_bound),
    explanatory_points = explanatory_points,
    transposed_response_points = t(response_points),
    weights_generator = weights_generator,
    number_of_iterations = number_of_iterations,
    local_rotation_modeler = local_rotation_modeler,
    allow_reflections = allow_reflections
  )
  
  list(
    concentration = optimization_info$minimum,
    objective = optimization_info$objective
  )

  #----
}

# Implements the cross-validating objective function.
#
# @param concentration A non negative scalar whose reciprocal value
# is proportional to the bandwidth applied while estimating
# a spherical regression model.
# @param transposed_response_points A \emph{3}-by-\emph{m} matrix whose columns
# contain the Cartesian coordinates of the response points corresponding
# to the explanatory points.
# @param local_rotation_modeler A model for a local rotation.
# It must be prototyped as having three arguments, \code{evaluation_point},
# \code{explanatory_points}, and \code{transposed_response_points},
# and returning a 3D rotation matrix.
#
# @inheritParams cross_validate_concentration
cross_validating_objective <- function(
  concentration,
  explanatory_points,
  transposed_response_points,
  weights_generator,
  number_of_iterations,
  local_rotation_modeler,
  allow_reflections
  ) {

  number_of_points <- nrow(explanatory_points)

  transposed_fitted_response_points <- matrix(
    nrow = 3,
    ncol = number_of_points
  )

  explanatory_point_weights <- weights_generator(
    evaluation_points = explanatory_points,
    explanatory_points = explanatory_points,
    concentration = concentration
  )

  for (i in 1:number_of_points) {

    current_transposed_response_points <-
      transposed_response_points[, -i]

    current_explanatory_points_sequence <-
      get_explanatory_points_sequence_seq(
        explanatory_points[-i, ],
        current_transposed_response_points,
        concentration,
        weights_generator,
        number_of_iterations,
        local_rotation_modeler,
        allow_reflections
    )

    iterative_fitted_response_points <- fit_iterative_regression(
      explanatory_points[i, ],
      current_explanatory_points_sequence,
      current_transposed_response_points,
      explanatory_point_weights[-i, i],
      local_rotation_modeler,
      allow_reflections
    )

    transposed_fitted_response_points[, i] <- iterative_fitted_response_points[number_of_iterations, ]
  }

  mean((transposed_fitted_response_points - transposed_response_points) ^ 2)
}

# Gets the sequence of explanatory points matrices needed to
# to implement Algorithm 1 for iterative rotation fitting.
#
# This function does not support parallel execution (sequential version).
#
# This function expects the response points in a transposed form,
# in order to enhance the performance of rotation estimations
# for one-term models.
#
# @param explanatory_points An \emph{m}-by-3 matrix whose rows contain
# the Cartesian coordinates of the explanatory points used to
# calculate the regression estimators.
# @param transposed_response_points A \emph{3}-by-\emph{m} matrix whose columns
# contain the Cartesian coordinates of the response points corresponding
# to the explanatory points.
# @param concentration A non negative scalar whose reciprocal value
# is proportional to the bandwidth applied while estimating
# a spherical regression model.
# @param weights_generator A function that, given a matrix of \emph{n}
# evaluation points, returns an \emph{m}-by-\emph{n} matrix whose
# \emph{j}-th column contains
# the weights assigned to the explanatory points while analyzing the
# \emph{j}-th evaluation point. Defaults to \code{\link{weight_explanatory_points}}.
# @param number_of_iterations The number of
# rotation fitting steps to be executed.
# At each step, the points estimated during the previous step
# are exploited as the current explanatory points. Defaults to \code{1}.
# @param local_rotation_modeler A model for a local rotation.
# It must be prototyped as having three arguments, \code{evaluation_point},
# \code{explanatory_points}, and \code{transposed_response_points},
# and returning a 3D rotation matrix.
# @param allow_reflections A logical scalar value. If set to \code{TRUE} signals
# that reflections are allowed. Defaults to \code{FALSE}. It is ignored if
# \code{number_of_expansion_terms} is \code{2}.
#
# @return A \code{number_of_iterations}-length vector of lists, with the \code{s}-th
# list having a single component,
# \code{explanatory_points}, an \emph{m}-by-\emph{3} matrix whose rows contain
# the Cartesian coordinates of the points exploited as explanatory at
# iteration \code{s}.
get_explanatory_points_sequence_seq <- function(
  explanatory_points,
  transposed_response_points,
  concentration,
  weights_generator,
  number_of_iterations,
  local_rotation_modeler,
  allow_reflections
) {
  explanatory_points_sequence <- vector(
    mode = "list",
    length = number_of_iterations
  )

  explanatory_points_sequence[[1]]$explanatory_points <- explanatory_points

  if (number_of_iterations > 1) {

    evaluation_specific_explanatory_point_weights <-
      weights_generator(
      explanatory_points,
      explanatory_points,
      concentration
    )

    number_of_explanatory_points <- nrow(explanatory_points)

    for (s in 2:number_of_iterations) {

      iteration_explanatory_points <- matrix(
        nrow = number_of_explanatory_points,
        ncol = 3
      )

      iteration_evaluation_points <- explanatory_points_sequence[[s - 1]]$explanatory_points

      for (i in 1:number_of_explanatory_points) {
        iteration_fitted_response_point <- fit_iterative_regression(
        iteration_evaluation_points[i, ],
        explanatory_points_sequence[s - 1],
        transposed_response_points,
        evaluation_specific_explanatory_point_weights[, i],
        local_rotation_modeler,
        allow_reflections)

        iteration_explanatory_points[i, ] <- iteration_fitted_response_point
      }
      explanatory_points_sequence[[s]]$explanatory_points <- iteration_explanatory_points
    }
  }
  explanatory_points_sequence
}