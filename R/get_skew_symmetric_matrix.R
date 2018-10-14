#' Gets a 3-by-3 Skew Symmetric Matrix.
#'
#' Returns the 3-by-3 skew symmetric matrix having the specified
#' independent components.
#'
#' Given a vector of components, say  \eqn{[x,y,z]}, this function
#' will return matrix
#'
#'\tabular{rrrrr}{
#'  \eqn{0} \tab \tab \eqn{-z} \tab \tab \eqn{y} \cr
#'  \eqn{z} \tab \tab \eqn{0}  \tab \tab \eqn{-x}\cr
#' \eqn{-y} \tab \tab \eqn{x}  \tab \tab \eqn{0}\cr}
#' @param independent_components A vector containing
#' the independent components of the matrix to get.
#'
#' @return The 3-by-3 skew symmetric matrix corresponding to
#' the specified independent components.
#' @export
#' @family Regression functions
#' @seealso \url{https://en.wikipedia.org/wiki/Skew-symmetric_matrix}.
#' @example examples/example_get_skew_symmetric_matrix.R
get_skew_symmetric_matrix <- function(independent_components) {

  # Input validation ---

  parameter_validators$independent_components$validate(
    independent_components, -4)

  # Implementation ---

    x <- independent_components[1]
    y <- independent_components[2]
    z <- independent_components[3]
    matrix(data = c(0, - z,   y,
                    z,   0, - x,
                  - y,   x,   0),
           byrow = TRUE,
           nrow = 3,
           ncol = 3)
}