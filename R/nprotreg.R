#' nprotreg: Nonparametric Rotations for Sphere-Sphere Regression.
#'
#' The nprotreg package provides several categories of functions.
#'
#' @section Regression functions:
#' Regression functions provide support for simulating and
#' fitting 3-dimensional spherical regression models.
#' \itemize{
#'   \item \code{\link{cross_validate_concentration}}
#'   \item \code{\link{fit_regression}}
#'   \item \code{\link{get_equally_spaced_points}}
#'   \item \code{\link{get_skew_symmetric_matrix}}
#'   \item \code{\link{simulate_regression}}
#'   \item \code{\link{simulate_rigid_regression}}
#'   \item \code{\link{weight_explanatory_points}}
#' }
#' @section Conversion functions:
#' Conversion functions transform coordinates of points
#' on a 3-dimensional sphere with unit radius and center at the origin.
#' \itemize{
#'   \item \code{\link{convert_cartesian_to_spherical}}
#'   \item \code{\link{convert_spherical_to_cartesian}}
#' }
#' @docType package
#' @name nprotreg
#' @aliases nprotreg-package
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom methods new
#' @importFrom stats nlm
#' @importFrom stats optimize
NULL