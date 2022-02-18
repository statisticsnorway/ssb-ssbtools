test_that("ModelMatrix: dimVar vs formula", {
  z3 <- SSBtoolsData("z3")
  
  set.seed(123)
  z <- z3[sample.int(nrow(z3),50), ]
  
  m1 <- ModelMatrix(z, formula = ~ region*hovedint*mnd + fylke*hovedint*mnd + kostragr*hovedint*mnd + region*hovedint*mnd2 + fylke*hovedint*mnd2 + kostragr*hovedint*mnd2, crossTable = TRUE)
  m2 <- ModelMatrix(z, formula = ~ region*hovedint*mnd + fylke*hovedint*mnd + kostragr*hovedint*mnd + region*hovedint*mnd2 + fylke*hovedint*mnd2 + kostragr*hovedint*mnd2, crossTable = TRUE, includeEmpty = TRUE)
  m3 <- ModelMatrix(z, dimVar = 1:6 , crossTable = TRUE)
  m4 <- ModelMatrix(z, dimVar = 1:6, crossTable = TRUE, removeEmpty = TRUE)
  
  ma14 <- Match(m1$crossTable, m4$crossTable)
  expect_equal(range(diff(sort(ma14))), c(1, 1))
  
  ma23 <- Match(m2$crossTable,m3$crossTable)
  expect_equal(range(diff(sort(ma23))), c(1, 1))
  
  expect_equal(max(abs(m4$modelMatrix[,ma14] -m1$modelMatrix)), 0)
  expect_equal(max(abs(m3$modelMatrix[,ma23] -m2$modelMatrix)), 0)
})
