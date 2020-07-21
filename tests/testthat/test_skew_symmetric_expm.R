library(nprotreg)
context("Exponential and Logaritmic Matrix Functions: expm")

test_that("internal_skew_simmetric_expm", {

  # Default use case scenario ---

  # Define a vector of independent components.

  independent_components <- cbind(1, 2, 3)

  # Get the corresponding 3-by-3 skew symmetric matrix.

  m <- get_skew_symmetric_matrix(independent_components)

  # Compute the corresponding exponential matrix.

  actual <- expm(m)

  # Assert

  expected <- rbind(
    cbind(-0.69492055764131177575, 0.71352099052778772403, 0.08929285886191218324),
    cbind(-0.19200697279199935297, -0.30378504433947051133, 0.93319235382364695841),
    cbind(0.69297816774177023458, 0.63134969938371787723, 0.34810747783026463331)
  )

  expect_equal(
        object = actual,
        expected = expected,
        tolerance = 1.0e-4
  )
})