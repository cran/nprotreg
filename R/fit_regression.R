#' Fits a 3D Spherical Regression.
#'
#' Returns 3D spherical points obtained by locally rotating
#' the specified evaluation
#' points, given an approximated model for local rotations
#' and a weighting scheme for the observed data set.
#' This function implements the method for sphere-sphere regression
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
#' @param evaluation_points An \emph{n}-by-3 matrix whose rows contain
#' the Cartesian coordinates of the points at which the regression
#' will be estimated.
#' @param explanatory_points An \emph{m}-by-3 matrix whose rows contain
#' the Cartesian coordinates of the explanatory points used to
#' calculate the regression estimators.
#' @param response_points An \emph{m}-by-\emph{3} matrix whose rows contain
#' the Cartesian coordinates of the response points corresponding
#' to the explanatory points.
#' @param concentration A non negative scalar whose reciprocal value
#' is proportional to the bandwidth applied while estimating
#' a spherical regression model.
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
#' @return A \code{number_of_iterations}-length vector of lists, with the \code{s}-th
#' list having two components,
#' \code{fitted_response_points}, an \emph{n}-by-\emph{3} matrix whose rows contain
#' the Cartesian coordinates of the fitted points at iteration \code{s}, and
#' \code{explanatory_points}, an \emph{m}-by-\emph{3} matrix whose rows contain
#' the Cartesian coordinates of the points exploited as explanatory at
#' iteration \code{s}.
#' @export
#' @family Regression functions
#' @example examples/example_fit_regression.R
#' @references Marco Di Marzio, Agnese Panzera & Charles C. Taylor (2018)
#' Nonparametric rotations for sphere-sphere regression,
#' Journal of the American Statistical Association,
#' DOI: \href{https://doi.org/10.1080/01621459.2017.1421542}{10.1080/01621459.2017.1421542}
fit_regression <- function(
  evaluation_points,
  explanatory_points,
  response_points,
  concentration,
  weights_generator = weight_explanatory_points,
  number_of_expansion_terms = 1,
  number_of_iterations = 1,
  allow_reflections = FALSE
) {
  # Input validation ---

  parameter_validators$evaluation_points$validate(evaluation_points, -4)
  parameter_validators$explanatory_points$validate(explanatory_points, -4)
  parameter_validators$response_points$validate(response_points, -4)

  parameter_validators$response_points$check_shared_number_of_rows(
    response_points,
    explanatory_points,
    parameter_validators$explanatory_points$name,
    -3
  )

  parameter_validators$concentration$validate(concentration, -4)
  fnc_typed_parameter_validators$weights_generator$validate(weights_generator, -4)

  parameter_validators$number_of_expansion_terms$validate(number_of_expansion_terms, -4)
  parameter_validators$number_of_iterations$validate(
    number_of_iterations,
    -4
  )

  parameter_validators$allow_reflections$validate(allow_reflections, -4)

  # Implementation ---

  local_rotation_modeler <- switch(
    number_of_expansion_terms,
    one_term_local_rotation_modeler,
    two_term_local_rotation_modeler
  )

  number_of_evaluation_points <- nrow(evaluation_points)

  transposed_response_points <- t(response_points)

  explanatory_point_weights <- weights_generator(
    evaluation_points,
    explanatory_points,
    concentration
  )

  explanatory_points_sequence <-
    get_explanatory_points_sequence(
      explanatory_points,
      transposed_response_points,
      concentration,
      weights_generator,
      number_of_iterations,
      local_rotation_modeler,
      allow_reflections
  )

  fit_info <- vector(mode = "list", length = number_of_iterations)
  for (s in 1:number_of_iterations) {
    fit_info[[s]] <- list(
      fitted_response_points = matrix(
        nrow = number_of_evaluation_points,
        ncol = 3),
      explanatory_points = explanatory_points_sequence[[s]]$explanatory_points
    )
  }

  for (i in 1:number_of_evaluation_points) {
    iterative_fitted_response_points <- fit_iterative_regression(
      evaluation_points[i, ],
      explanatory_points_sequence,
      transposed_response_points,
      explanatory_point_weights[, i],
      local_rotation_modeler,
      allow_reflections
    )
    for (s in 1:number_of_iterations) {
      fit_info[[s]]$fitted_response_points[i, ] <- iterative_fitted_response_points[s, ]
    }
  }
  fit_info

  #----
}

# Implements and generalizes Algorithm 1 for iterative rotation fitting.
#
# This is a generalization of the algorithm
# Di Marzio et al. (2018) refer to as 1.
# Here it is parameterized w.r.t.
# to a generic local rotation modeler.
#
# This function expects the response points in a transposed form,
# in order to enhance the performance of rotation estimations
# for one-term models.
#
# @param evaluation_point A row containing
# the Cartesian coordinates of the point at which the regression
# will be estimated.
# @param explanatory_points_sequence A \code{number_of_iterations}-length
# vector of lists having the single component \code{explanatory_points}.
# In the list at position \code{s}, it is the \emph{m}-by-3 matrix whose rows contain
# the Cartesian coordinates of the explanatory points used to
# calculate the regression estimators at iteration \code{s}.
# @param transposed_response_points A \emph{3}-by-\emph{m} matrix whose columns
# contain the Cartesian coordinates of the response points corresponding
# to the explanatory points.
# @param evaluation_specific_explanatory_point_weights
# An \eqn{m}-by-\eqn{1} vector whose entries
# store the weights assigned to the observed
# \code{explanatory_points} while estimating the regression
# at the given \code{evaluation_point}.
# @param local_rotation_modeler A model for a local rotation.
# It must be prototyped as having three arguments, \code{evaluation_point},
# \code{explanatory_points}, and \code{transposed_response_points},
# and returning a 3D rotation matrix.
# @param allow_reflections A logical scalar value. If set to \code{TRUE} signals
# that reflections are allowed. Defaults to \code{FALSE}. It is ignored if
# \code{number_of_expansion_terms} is \code{2}.
#
# @return A \code{number_of_iterations}-by-\eqn{3} matrix whose \code{s}-th row
# contains the response obtained applying the given rotation model
# to the specified \code{evaluation_point} at iteration \code{s}.
fit_iterative_regression <- function(
  evaluation_point,
  explanatory_points_sequence,
  transposed_response_points,
  evaluation_specific_explanatory_point_weights,
  local_rotation_modeler,
  allow_reflections
) {

  number_of_iterations <- length(explanatory_points_sequence)

  iterative_fitted_response_points <- matrix(
    nrow = number_of_iterations,
    ncol = 3
  )

  cumulative_rotation <- diag(3)
  for (s in 1:number_of_iterations) {

    additional_rotation <- local_rotation_modeler(
        evaluation_point,
        explanatory_points_sequence[[s]]$explanatory_points,
        transposed_response_points,
        evaluation_specific_explanatory_point_weights,
        allow_reflections)

    cumulative_rotation <- additional_rotation %*% cumulative_rotation
    iterative_fitted_response_points[s, ] <- cumulative_rotation %*% evaluation_point
  }
  iterative_fitted_response_points
}

# Gets the sequence of explanatory points matrices needed to
# to implement Algorithm 1 for iterative rotation fitting.
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
get_explanatory_points_sequence <- function(
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
        iteration_evaluation_points[i, ], #explanatory_points[i, ],
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

# Models Rotations With One-Term Local Expansions.
#
# This is an implementation of the one-term local rotation
# model proposed by Di Marzio et al. (2018).
#
# @param evaluation_point A row containing
# the Cartesian coordinates of the point at which the regression
# will be estimated.
# @param explanatory_points An \emph{m}-by-3 matrix whose rows contain
# the Cartesian coordinates of the explanatory points used to
# calculate the regression estimators.
# @param transposed_response_points A \emph{3}-by-\emph{m} matrix whose columns
# contain the Cartesian coordinates of the response points corresponding
# to the explanatory points.
# @param evaluation_specific_explanatory_point_weights
# An \eqn{m}-by-\eqn{1} vector whose entries
# store the weights assigned to the observed
# \code{explanatory_points} while estimating the regression
# at the given \code{evaluation_point}.
# @param allow_reflections A logical scalar value. If set to \code{TRUE} signals
# that reflections are allowed. Defaults to \code{FALSE}. It is ignored if
# \code{number_of_expansion_terms} is \code{2}.
#
# @return The modeled rotation matrix local to the specified
# \code{evaluation_point}.
one_term_local_rotation_modeler <- function(
  evaluation_point,
  explanatory_points,
  transposed_response_points,
  evaluation_specific_explanatory_point_weights,
  allow_reflections
) {

  svd_info <- svd(
    transposed_response_points %*%
    diag(evaluation_specific_explanatory_point_weights) %*%
    explanatory_points
  )

  u <- svd_info$u
  transposed_v <- t(svd_info$v)
  local_rotation <- u %*% transposed_v

  if (!allow_reflections) {
    local_rotation_det <- det(local_rotation)
    local_rotation <- u %*% diag(c(1, 1, local_rotation_det)) %*% transposed_v
  }
  local_rotation
}

# Models Rotations With Two-Term Local Expansions.
#
# This is an implementation of the two-term local rotation
# model proposed by Di Marzio et al. (2018).
#
# @param evaluation_point A row containing
# the Cartesian coordinates of the point at which the regression
# will be estimated.
# @param explanatory_points An \emph{m}-by-3 matrix whose rows contain
# the Cartesian coordinates of the explanatory points used to
# calculate the regression estimators.
# @param transposed_response_points A \emph{3}-by-\emph{m} matrix whose columns
# contain the Cartesian coordinates of the response points corresponding
# to the explanatory points.
# @param evaluation_specific_explanatory_point_weights
# An \eqn{m}-by-\eqn{1} vector whose entries
# store the weights assigned to the observed
# \code{explanatory_points} while estimating the regression
# at the given \code{evaluation_point}.
# @param allow_reflections A logical scalar value. If set to \code{TRUE} signals
# that reflections are allowed. Defaults to \code{FALSE}. It is ignored if
# \code{number_of_expansion_terms} is \code{2}.
#
# @return The modeled rotation matrix local to the specified
# \code{evaluation_point}.
two_term_local_rotation_modeler <- function(
  evaluation_point,
  explanatory_points,
  transposed_response_points,
  evaluation_specific_explanatory_point_weights,
  allow_reflections
) {
  # Function crude2 ---

  # Computes The 3D Weighted Least Squares As A Function Of 12 components.
  #
  # @param components A 12-length vector interpreted as the argument
  # of a function whose value is
  # the Weighted Least Squares expression.
  #
  # @return The WLS for the specified \code{components}.
  #
  # @inheritParams fit_iterative_regression
  final_objective_function <- function(
    evaluation_point,
    explanatory_points,
    transposed_response_points,
    evaluation_specific_explanatory_point_weights,
    components
  ) {

    # This is equivalent to function 'crude2' in legacy code.

    r <- components # matrix(components, 3, 4)
    dim(r) <- c(3, 4)
    w <- evaluation_specific_explanatory_point_weights
    x <- explanatory_points
    y <- t(transposed_response_points)
    transposed_x <- t(x)
    number_of_points <- nrow(x)
    id <- diag(3)
    yhat <- matrix(0.0, nrow = number_of_points, ncol = 3)
    dd <- rbind(1, evaluation_point - transposed_x)
    r1 <- r %*% dd
    for (i in 1:number_of_points) {
      current_x <- transposed_x[, i]
      yhat[i, ] <- current_x

      if (!all(r1[, i] == 0)) {
        s <- get_skew_symmetric_matrix(r1[, i])
        s_norm <- sqrt(s[1, 2] ^ 2 + s[1, 3] ^ 2 + s[2, 3] ^ 2)
        s <- s / s_norm
        local_rotation <- id + sin(s_norm) * s + (1 - cos(s_norm)) * s %*% s
        yhat[i, ] <- local_rotation %*% current_x
      }
    }
    value <- sum(w * (y - yhat) ^ 2)
  }

  # Implementation ---

  r <- one_term_local_rotation_modeler(
    evaluation_point,
    explanatory_points,
    transposed_response_points,
    evaluation_specific_explanatory_point_weights,
    allow_reflections
  )
  s0 <- logm(r)
  rr <- c(-s0[2, 3], s0[1, 3], -s0[1, 2])

  pp <- c(rr, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  nn1 <- nlm(
    final_objective_function,
    p = pp,
    evaluation_point = evaluation_point,
    explanatory_points = explanatory_points,
    transposed_response_points = transposed_response_points,
    evaluation_specific_explanatory_point_weights =
      evaluation_specific_explanatory_point_weights
  )
  s <- get_skew_symmetric_matrix(nn1$est[1:3])
  s_norm <- sqrt(s[1, 2] ^ 2 + s[1, 3] ^ 2 + s[2, 3] ^ 2)
  s <- s / s_norm
  local_rotation <- diag(3) + sin(s_norm) * s + (1 - cos(s_norm)) * s %*% s

  # ----
}

# Computes Weighted Least Squares For 3D Regressions.
#
# This is the objective function of the problem
# described in Equation (9).
#
# @param local_rotation A 3-by-3 rotation matrix.
# @param explanatory_points An \emph{m}-by-3 matrix whose rows contain
# the Cartesian coordinates of the explanatory points used to
# calculate the regression estimators.
# @param transposed_response_points A \emph{3}-by-\emph{m} matrix whose columns
# contain the Cartesian coordinates of the response points corresponding
# to the explanatory points.
# @param evaluation_specific_explanatory_point_weights
# An \eqn{m}-by-\eqn{1} vector whose entries
# store the weights assigned to the observed
# \code{explanatory_points} while estimating the regression
# at the given \code{evaluation_point}.
# @return The weighted least squares corresponding
# to the specified parameters.
weighted_least_squares <- function(
  local_rotation,
  explanatory_points,
  transposed_response_points,
  evaluation_specific_explanatory_point_weights
) {
  r <- local_rotation
  w <- evaluation_specific_explanatory_point_weights
  x <- explanatory_points
  yT <- transposed_response_points
  hT <- yT - t(x %*% r)
  sum(diag(hT %*% diag(w) %*% t(hT)))
}
