

test_that("forward_dots and dots2dots", {
  expect_equal(2 * c(f5(1), f5(1, 2, 3), f5(1, 2, 4)), 
               amf())
  expect_equal(2 * c(f5(1), f5(1, 2, 3), f5(1, 2, 4)), 
               amf(a3 = 10, a5 = 100))
  expect_equal(2 * c(f5(1, 0, 10), f5(1, 2, 10, 3, 100), f5(1, 2, 10, 4, 100)), 
               amf(a3 = 10, a5 = 100, forward_dots = TRUE))
  expect_equal(2 * c(f5(1, 0, 10, a5 = 100), f5(1, 2, 10, 3, 100), f5(1, 2, 10, 4, 100)), 
               amf(a3 = 10, a5 = 100, forward_dots = TRUE, dots2dots = TRUE))
  expect_equal(2 * c(f5(1), f5(1, 2, 10, 3, 100), f5(1, 2, 10, 4, 100)), 
               amf(a3 = 10, a5 = 100, forward_dots = c(FALSE, TRUE)))
  expect_equal(2 * c(f5(1, 0, 10), f5(1, 2, 3), f5(1, 2, 4)), 
               amf(a3 = 10, a5 = 100, forward_dots = c(TRUE, FALSE)))
  expect_equal(2 * c(f5(1, 0, 10), f5(1, 2, 3), f5(1, 2, 4)), 
               amf(a3 = 10, a5 = 100, forward_dots = c(TRUE, FALSE), dots2dots = c(FALSE, TRUE)))
  expect_equal(2 * c(f5(1, 0, 10, a5 = 100), f5(1, 2, 3), f5(1, 2, 4)), 
               amf(a3 = 10, a5 = 100, forward_dots = c(TRUE, FALSE), dots2dots = c(TRUE, FALSE)))
})
