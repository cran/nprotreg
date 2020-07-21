library(nprotreg)
context("Exponential and Logaritmic Matrix Functions: logm")

test_that("internal_rotation_logm", {

  # Default use case scenario ---

  # Define a rotation matrix.

  m <- rbind(
    cbind(-0.69492055764131177575, 0.71352099052778772403, 0.08929285886191218324),
    cbind(-0.19200697279199935297, -0.30378504433947051133, 0.93319235382364695841),
    cbind(0.69297816774177023458, 0.63134969938371787723, 0.34810747783026463331)
  )

  # Compute the corresponding logarithm matrix.

  actual <- logm(m)

  # Assert

  expected <- rbind(
    cbind(-2.201598e-15, 2.037756e+00, -1.358504e+00),
    cbind(-2.037756e+00, 2.263298e-15, 6.792519e-01),
    cbind(1.358504e+00, -6.792519e-01, 1.270568e-15)
  )

  expect_equal(
        object = actual,
        expected = expected,
        tolerance = 1.0e-4
  )
})