test_that("DummyApply works", {
  z <- SSBtools::SSBtoolsData("sprt_emp_withEU")
  
  x <- SSBtools::ModelMatrix(z, formula = ~age + geo, crossTable = TRUE)$modelMatrix
  sum1 <- (t(x) %*% z$ths_per)[, 1]
  sum2 <- DummyApply(x, z$ths_per, sum)
  
  expect_equal(sum1, sum2, check.attributes = FALSE)
  
  x[, 1] <- 0
  
  max1 <- suppressWarnings(DummyApply(as.matrix(x), z$ths_per, max))
  max2 <- suppressWarnings(DummyApply(x, z$ths_per, range)[, 2])
  
  expect_equal(max1, max2)
})
