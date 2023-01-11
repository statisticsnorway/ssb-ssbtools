
# see  tests/testthat/helper-aggregate_multiple_fun.R


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


test_that("model_aggregate and more", {
  ma0 <- c(51.33333, 149.8, 149.8, 17.11111, 16.11111, 18.11111)
  ma30 <- c(51.33333, 149.8, 149.8, 17.11111, 16.111, 18)
  ma34a <- c(51.33333, 149.8, 149.8, 17.11111, 16.111, 18.1111)
  ma34b <- c(51.33333, 149.8, 149.8 + TRUE + 3 + 4, 17.11111, 16.111, 18.1111)
  
  expect_equal(ma0, ma())
  expect_equal(ma0, ma(dim_var = c("age", "year"), formula = NULL))
  expect_equal(ma0, ma(formula = ~age * year))
  expect_equal(ma0, ma(formula = ~age * year,   pre_aggregate = FALSE))
  expect_equal(ma0, ma(do_round = TRUE, mdigits = 3, digits = 4))
  expect_equal(ma30, ma(do_round = TRUE, mdigits = 3, digits = 4, forward_dots = TRUE))
  expect_equal(ma34a, ma(do_round = TRUE, mdigits = 3, digits = 4, forward_dots = TRUE, dots2dots = c(FALSE, TRUE, TRUE)))
  expect_equal(ma34a, ma(do_round = TRUE, mdigits = 3, digits = 4, forward_dots = c(FALSE, TRUE, TRUE), dots2dots = TRUE))
  expect_equal(ma34b, ma(do_round = TRUE, mdigits = 3, digits = 4, forward_dots = TRUE, dots2dots = TRUE))
  
  out1 <- ma(frame_return = TRUE)
  
  out2 <- aggregate_multiple_fun(za, by = za[c("age", "year")], vars = c(sum = "ths", mean = "y", ra = "y"), fun = c(sum = sum, mean = mean, ra = my_range2))
  
  expect_equal(sum(is.finite(Match(out2, out1[names(out2)]))), 6)
  
  out3 <- unmatrix(aggregate(za[c("ths", "y")], za[c("age", "year")], my_range2)[, -3])
  
  names(out3)[3:4] <- names(out2)[5:6]
  expect_equal(out2[, -c(3:4)], out3)
  expect_equal(unique(sapply(ma(formula = ~age * year, pre_aggregate = TRUE, pre_return = TRUE, frame_return = TRUE), nrow)), 6)
  expect_equal(nrow(ma(formula = ~age * year, pre_aggregate = FALSE, pre_return = TRUE, frame_return = TRUE)[[1]]), 18)
  
})