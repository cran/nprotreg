#' Computes the Logarithm of a 3D Rotation Matrix.
#'
#' @param rotation_matrix A 3-by-3 rotation matrix.
#'
#' @return A 3-by-3 skew-symmetric matrix representing the logarithm 
#' of the specified rotation matrix.
logm <- function(rotation_matrix) {

  # Internal function: no input validation ---

  # Implementation ---

  r <- rotation_matrix
  r_trace <- r[1, 1] + r[2, 2] + r[3, 3];
  if (r_trace == 0) {
    skew_symmetric_matrix <- matrix(0, 3, 3);
  } else {
    theta <- acos((r_trace - 1) / 2)
    skew_symmetric_matrix <- theta * (r - t(r)) / 2 / sin(theta)
  }
  skew_symmetric_matrix
}