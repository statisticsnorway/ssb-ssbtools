test_that("GaussSuppression with removeDuplicated ", {
  
  # Input data as in example
  df <- data.frame(values = c(1, 1, 1, 5, 5, 9, 9, 9, 9, 9, 0, 0, 0, 7, 7), 
                   var1 = rep(1:3, each = 5), 
                   var2 = c("A", "B", "C", "D", "E"), stringsAsFactors = FALSE)
  
  x <- ModelMatrix(df, formula = ~var1 * var2)
  xx <- cbind(x, x)
  
  expect_warning({a <- GaussSuppression(xx, primary = 24 + 10:12, forced = 10, removeDuplicated = FALSE, printInc = FALSE)})
  expect_warning({b <- GaussSuppression(xx, primary = 24 + 10:12, forced = 10, removeDuplicated = TRUE, printInc = FALSE)})
  expect_equal(a, b)
  
  xx[, 10] <- 0
  
  a <- GaussSuppression(xx, primary = 24 + 10:12, forced = 10, removeDuplicated = FALSE, printInc = FALSE)
  b <- GaussSuppression(xx, primary = 24 + 10:12, forced = 10, removeDuplicated = TRUE, printInc = FALSE)
  expect_equal(a, b)

  xx[, 34] <- 0

  expect_warning({a <- GaussSuppression(xx, primary = 24 + 10:12, forced = 10, removeDuplicated = FALSE, printInc = FALSE)})
  expect_warning({b <- GaussSuppression(xx, primary = 24 + 10:12, forced = 10, removeDuplicated = TRUE, printInc = FALSE)})
  expect_equal(a, b)
  
})
