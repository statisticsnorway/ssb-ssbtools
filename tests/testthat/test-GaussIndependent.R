test_that("GaussIndependent works", {
  x <- ModelMatrix(SSBtoolsData("z2"), formula = ~fylke + kostragr * hovedint - 1)
  a <- GaussIndependent(x)
  expect_equivalent(which(a$rows), c(1, 2, 3, 5, 7, 9, 10, 12, 20, 23, 31, 34, 42))
  expect_equivalent(which(a$columns), c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 13, 14, 15))
  b <- GaussIndependent(t(x))
  expect_identical(a$rows, b$columns)
  expect_identical(b$rows, a$columns)
  
  x <- as.matrix(x)
  x[113:537] <- rev(x[113:537])
  x[5, ] <- 0
  x[, 14] <- x[, 5] + x[, 6]
  x[42, ] <- x[1, ] + x[36, ]
  
  a <- GaussIndependent(x)
  expect_equivalent(which(a$rows), c(1, 2, 3, 4, 6, 8, 9, 10, 12, 16, 19, 20, 23, 31, 34, 36, 37, 43))
  expect_equivalent(which(a$columns), c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20))
  b <- GaussIndependent(t(x))
  expect_identical(a$rows, b$columns)
  expect_identical(b$rows, a$columns)
  
})


# Sample with seed inside test_that do not work
k <- 1:20000
set.seed(123)
sample_k <- sample(k)

test_that("GaussIndependent integer overflow", {
  z3 <- SSBtoolsData("z3")
  x <- SSBtools::ModelMatrix(z3[, 1:6], sparse = FALSE)
  x[k] <- x[sample_k]
  x <- x[, order(crossprod(x,z3$ant), decreasing = TRUE)]
  expect_error({a <- suppressWarnings(GaussIndependent(x))})
})

