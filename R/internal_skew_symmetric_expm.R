#' Computes the Exponential of a 3D Skew Symmetric Matrix.
#'
#' The exponential of a skew-symmetric matrix is computed by means
#' of the Rodrigues' formula.
#'
#' @param skew_symmetric_matrix A 3-by-3 skew-symmetric matrix.
#'
#' @return A 3-by-3 rotation matrix representing the exponential 
#' of the specified skew-symmetric matrix.
expm <- function(skew_symmetric_matrix) {

  # Internal function: no input validation ---

  # Implementation ---

  k <- skew_symmetric_matrix
  theta <- sqrt(k[1, 2] ^ 2 + k[1, 3] ^ 2 + k[2, 3] ^ 2)
  m <- k / theta
 
  diag(3) + sin(theta) * m + (1 - cos(theta)) * m %*% m
}