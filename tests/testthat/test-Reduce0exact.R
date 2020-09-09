test_that("Reduce0exact works", {
  # Make a special data set
  d <- SSBtoolsData("sprt_emp")
  d$ths_per <- round(d$ths_per)
  d <- rbind(d, d)
  d$year <- as.character(rep(2014:2019, each = 6))
  to0 <- rep(TRUE, 36)
  to0[c(6, 14, 17, 18, 25, 27, 30, 34, 36)] <- FALSE
  d$ths_per[to0] <- 0
  
  # Values as a single column matrix
  y <- Matrix(d$ths_per, ncol = 1)
  
  # A model matrix using a special year hierarchy
  x <- Hierarchies2ModelMatrix(d, hierarchies = list(geo = "", age = "", year = 
    c("y1418=2014+2015+2016+2017+2018", "y1519=2015+2016+2017+2018+2019", "y151719=2015+2017+2019")
     ), inputInOutput = FALSE)
  
  # Aggregates 
  z <- t(x) %*% y
  
  a <- Reduce0exact(x, z)
  expect_equivalent(sum(a$yKnown), 17)
  
  a <- Reduce0exact(x, z, reduceByColSums = TRUE)
  expect_equivalent(max(abs((y-a$y)[which(a$yKnown)])), 0)
  expect_equivalent(dim(a$x), c(18,11))
 
  a <- Reduce0exact(x, z, reduceByLeverage = TRUE)
  expect_equivalent(max(abs((y-a$y)[which(a$yKnown)])), 0)
  expect_equivalent(dim(a$x), c(14,11))
  expect_s4_class(a$x,"dgCMatrix")
  expect_s4_class(a$y,"dgCMatrix")
  
  b <- Reduce0exact(as.matrix(x), z, y = y, reduceByLeverage = TRUE)
  expect_true(is.matrix(b$x))
  expect_s4_class(b$y,"dgCMatrix")
  expect_equivalent(as.matrix(a$x), b$x) 
  expect_equivalent(sum(abs((z- t(x) %*% b$y)[which(!b$zSkipped),1, drop=FALSE]-b$z)),0)
  expect_equal(rownames(b$z), colnames(x)[which(!b$zSkipped)])
  
  a <- Reduce0exact(x, y = as.matrix(y), reduceByLeverage = TRUE)
  expect_true(is.matrix(a$y))
  expect_s4_class(a$x,"dgCMatrix")
  expect_equivalent(as.matrix(a$x), b$x) 
  expect_equivalent(as.matrix(b$y), a$y)
  
  b <- Reduce0exact(cbind(tot=1,x), y = y, reduceByLeverage = TRUE)
  expect_equivalent(dim(b$x), c(10,11))
  expect_equivalent(max(abs((y-b$y)[which(a$yKnown)])), 0)
  
  
})
