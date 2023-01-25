
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
  expect_equal(unique(sapply(ma(formula = ~age * year, pre_aggregate = TRUE, pre_return = TRUE, preagg_var = "eu", frame_return = TRUE), nrow)), 12)
  expect_equal(nrow(ma(formula = ~age * year, pre_aggregate = FALSE, pre_return = TRUE, frame_return = TRUE)[[1]]), 18)
  expect_equal(nrow(formula_selection(out1, "geo")), 3)
  
  expect_equal(ma0[-c(1:2)], ma(sum_vars = integer(0)))
  expect_equal(ma30[-c(1:2)], ma(do_round = TRUE, mdigits = 3, digits = 4, forward_dots = TRUE, sum_vars = NULL))
  expect_equal(ma0[1:2], ma(fun_vars = character(0)))
  expect_true(is.null(ma(formula = ~age * year, pre_aggregate = TRUE, pre_return = TRUE, frame_return = TRUE, sum_vars = NULL)[["pre_sum"]]))
  expect_equal(dim(ma(formula = ~age * year, pre_aggregate = TRUE, pre_return = TRUE, frame_return = TRUE, fun_vars = NULL)[["pre_data"]]), c(6, 2))
  
  out4 <- ma(formula = ~age * eu + geo, frame_return = TRUE)
  expect_equal(formula_selection(out4, ~age:eu + age), formula_selection(out4, c("(Intercept)", "age", "age:eu")))
  expect_equal(formula_selection(out4, ~age * eu - 1), formula_selection(out4, c("eu*age")))
  expect_equal(formula_selection(out4, ~age * eu), formula_selection(out4, c("1", "age", "eu", "eu:age")))
})




test_that("1,2,3,4,5,6,7 variables, dummy and non-dummy", {
  
  z2 <- SSBtoolsData("z2")
  set.seed(12)
  z2$y <- round(rnorm(nrow(z2)), 2)
  z <- z2[sample.int(nrow(z2), size = 20), ]
  z$d <- 1
  z$e <- 2
  z$f <- 3
  z$g <- 4
  z$h <- 5
  
  x <- ModelMatrix(z, formula = ~hovedint:kostragr - 1)
  
  s1 <- as.vector(t(x) %*% z[["y"]])
  
  expect_equal(s1, 
               dummy_aggregate(data = z, x = x, 
                               fun = function(y) sum(y), 
                               vars = list(c("y")))$y)
  expect_equal(s1+1, 
               dummy_aggregate(data = z, x = x, 
                               fun = function(a1, y) sum(y) + max(a1), 
                               vars = list( y = list(c("d", "y"))))$y)
  expect_equal(s1+1+2, 
               dummy_aggregate(data = z, x = x, 
                               fun = function(a1, a2, y) sum(y) + max(a1+a2), 
                               vars = list( y = list(c("d", "e", "y"))))$y)
  expect_equal(s1+1+2+3, 
               dummy_aggregate(data = z, x = x, 
                               fun = function(a1, a2, a3, y) sum(y) + max(a1+a2+a3), 
                               vars = list( y = list(c("d", "e", "f", "y"))))$y)  
  expect_equal(s1+1+2+3+4, 
               dummy_aggregate(data = z, x = x, 
                               fun = function(a1, a2, a3, a4, y) sum(y) + max(a1+a2+a3+a4), 
                               vars = list( y = list(c("d", "e", "f", "g", "y"))))$y)
  expect_equal(s1+1+2+3+4+5, 
               dummy_aggregate(data = z, x = x, 
                               fun = function(a1, a2, a3, a4, a5, y) sum(y) + max(a1+a2+a3+a4+a5), 
                               vars = list( y = list(c("d", "e", "f", "g", "h", "y"))))$y)
  
  expect_equal(s1+1+2+3+4+5+3, 
               dummy_aggregate(data = z, x = x, 
                               fun = function(a1, a2, a3, a4, a5, a6, y) sum(y) + max(a1+a2+a3+a4+a5+a6), 
                               vars = list( y = list(c("d", "e", "f", "g", "h", "f", "y"))))$y)
  
  
  
  # Make a non-dummy matrix 
  x2 <- x
  x2[17, 2:5] <- c(-1, 3, 0, 10)
  x2[, 4] <- 0
  
  
  expect_equal(as.vector(t(x2^2) %*% z[["y"]]), 
               dummy_aggregate(data = z, x = x2, dummy = FALSE,
                               fun = function(x, y2) {sum(x^2 * y2)},  
                               vars = list( y = list(c("y"))))$y)
  
  
  s2 <- as.vector(t(x2) %*% z[["ant"]]  + t(x2^2) %*% z[["y"]])
  
  
  expect_equal(s2, 
               dummy_aggregate(data = z, x = x2, dummy = FALSE,
                               fun = function(x, y1, y2) {sum(x * y1) + sum(x^2 * y2)},  
                               vars = list( y = list(c("ant", "y"))))$y)
  
  s2[4] <- -Inf # since  max(integer(0)) = -Inf
  
  expect_equal(s2+1, 
               dummy_aggregate(data = z, x = x2, dummy = FALSE,
                               fun = function(x, a1, y1, y2) {
                                 sum(x * y1) + sum(x^2 * y2) + 
                                   suppressWarnings(max(a1))},  
                               vars = list( y = list(c("d", "ant", "y"))))$y)
  expect_equal(s2+1+2, 
               dummy_aggregate(data = z, x = x2, dummy = FALSE,
                               fun = function(x, a1, a2, y1, y2) {
                                 sum(x * y1) + sum(x^2 * y2) + 
                                   suppressWarnings(max(a1+a2))},  
                               vars = list( y = list(c("d", "e", "ant", "y"))))$y)
  expect_equal(s2+1+2+3, 
               dummy_aggregate(data = z, x = x2, dummy = FALSE,
                               fun = function(x, a1, a2, a3, y1, y2) {
                                 sum(x * y1) + sum(x^2 * y2) + 
                                   suppressWarnings(max(a1+a2+a3))},  
                               vars = list( y = list(c("d", "e", "f", "ant", "y"))))$y)
  expect_equal(s2+1+2+3+4, 
               dummy_aggregate(data = z, x = x2, dummy = FALSE,
                               fun = function(x, a1, a2, a3, a4, y1, y2) {
                                 sum(x * y1) + sum(x^2 * y2) + 
                                   suppressWarnings(max(a1+a2+a3+a4))},  
                               vars = list( y = list(c("d", "e", "f", "g", "ant", "y"))))$y)
  
  expect_equal(s2+1+2+3+4+5, 
               dummy_aggregate(data = z, x = x2, dummy = FALSE,
                               fun = function(x, a1, a2, a3, a4, a5, y1, y2) {
                                 sum(x * y1) + sum(x^2 * y2) + 
                                   suppressWarnings(max(a1+a2+a3+a4+a5))},  
                               vars = list( y = list(c("d", "e", "f", "g", "h", "ant", "y"))))$y)
  
  
  
})
