test_that("ModelMatrix: dimVar vs formula", {
  z3 <- SSBtoolsData("z3")
  
  set.seed(123)
  z <- z3[sample.int(nrow(z3),50), ]
  
  m1 <- ModelMatrix(z, formula = ~ region*hovedint*mnd + fylke*hovedint*mnd + kostragr*hovedint*mnd + region*hovedint*mnd2 + fylke*hovedint*mnd2 + kostragr*hovedint*mnd2, crossTable = TRUE)
  m2 <- ModelMatrix(z, formula = ~ region*hovedint*mnd + fylke*hovedint*mnd + kostragr*hovedint*mnd + region*hovedint*mnd2 + fylke*hovedint*mnd2 + kostragr*hovedint*mnd2, crossTable = TRUE, removeEmpty = FALSE)
  m3 <- ModelMatrix(z, dimVar = 1:6 , crossTable = TRUE)
  m4 <- ModelMatrix(z, dimVar = 1:6, crossTable = TRUE, removeEmpty = TRUE)
  
  ma14 <- Match(m1$crossTable, m4$crossTable)
  expect_equal(range(diff(sort(ma14))), c(1, 1))
  
  ma23 <- Match(m2$crossTable,m3$crossTable)
  expect_equal(range(diff(sort(ma23))), c(1, 1))
  
  expect_equal(max(abs(m4$modelMatrix[,ma14] -m1$modelMatrix)), 0)
  expect_equal(max(abs(m3$modelMatrix[,ma23] -m2$modelMatrix)), 0)
})


test_that("Parameters to FormulaSums + FormulaSelection", {
  
  skip_on_cran()
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    skip()
  }
  
  z3 <- SSBtoolsData("z3")
  
  set.seed(123)
  z <- z3[sample.int(nrow(z3), 50), ]
  for (i in 1:4) z[sample.int(nrow(z), 3), i] <- NA
  z$mnd2[!is.na(z$region) & !is.na(z$hovedint)] <- NA
  
  f <- ~fylke * hovedint * mnd + region * hovedint * mnd2 + kostragr * hovedint * mnd2
  
  ncols <- integer(0)
  
  for (removeEmpty in c(FALSE, TRUE)) {
    for (NAomit in c(FALSE, TRUE)) {
      m1 <- ModelMatrix(z, formula = f, 
                        crossTable = TRUE, 
                        removeEmpty = removeEmpty, 
                        NAomit = NAomit)
      ncols <- c(ncols,  
                 ncol(formula_selection(m1$modelMatrix, ~region:hovedint:mnd2 - 1)),
                 ncol(FormulaSelection(m1$modelMatrix, ~kostragr * hovedint * mnd2)))
      for (rowGroupsPackage in c("base", "data.table")) {
        for (viaSparseMatrix in c(FALSE, TRUE)) {
          m2 <- ModelMatrix(z, formula = f, 
                            crossTable = TRUE, 
                            removeEmpty = removeEmpty, 
                            NAomit = NAomit, 
                            rowGroupsPackage = rowGroupsPackage,
                            viaSparseMatrix = viaSparseMatrix)
          expect_equal(m1, m2)
        }
      }
    }
  }
  expect_equal(ncols, c(310, 126, 0, 44, 47, 80, 0, 39))
})



test_that("ModelMatrix: select parameter", {
  z <- SSBtoolsData("sprt_emp_withEU")
  z$age[z$age == "Y15-29"] <- "young"
  z$age[z$age == "Y30-64"] <- "old"
  ageHier <- data.frame(mapsFrom = c("young", "old"), mapsTo = "Total", sign = 1)
  geoDimList <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
  
  mm <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = "Total"), crossTable = TRUE)
  
  sel <- c(10, 3, 55, 57, 36, 33, 65, 61, 24, 58)
  selectA <- mm$crossTable[sel, ]
  selectA$year[2] <- "2023"
  selectA$age[3] <- "middle"
  selectB <- selectA[c(1, 4:10), ]
  selectC <- selectA[c(1, 1:10, 3, 4), ]
  
  
  selectD <- list(age = c("Total", "Total", "middle", "young"),
                  geo = c("nonEU", "Europe", "Portugal"), 
                  year = c("2014", "2023", "Total"))
  
  selectE <- selectD[1] 
  
  
  select <- selectA
  m1A <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), crossTable = TRUE, select = select, removeEmpty = TRUE)
  m2A <- ModelMatrix(z, formula = ~age * geo * year, crossTable = TRUE, select = select, removeEmpty = TRUE)
  m3A <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), formula = ~age * geo + year, crossTable = TRUE, select = select, removeEmpty = TRUE)
  m1A_ <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), crossTable = TRUE, select = select, removeEmpty = FALSE)
  expect_warning({m2A_ <- ModelMatrix(z, formula = ~age * geo * year, crossTable = TRUE, select = select, removeEmpty = FALSE)})
  expect_warning({m3A_ <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), formula = ~age * geo + year, crossTable = TRUE, select = select, removeEmpty = FALSE)})
  
  select <- selectB
  m1B <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), crossTable = TRUE, select = select, removeEmpty = TRUE)
  m2B <- ModelMatrix(z, formula = ~age * geo * year, crossTable = TRUE, select = select, removeEmpty = TRUE)
  m3B <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), formula = ~age * geo + year, crossTable = TRUE, select = select, removeEmpty = TRUE)
  m1B_ <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), crossTable = TRUE, select = select, removeEmpty = FALSE)
  m2B_ <- expect_warning({ModelMatrix(z, formula = ~age * geo * year, crossTable = TRUE, select = select, removeEmpty = FALSE)})
  m3B_ <- expect_warning({ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), formula = ~age * geo + year, crossTable = TRUE, select = select, removeEmpty = FALSE)})
  
  select <- selectC
  expect_warning({m1C <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), crossTable = TRUE, select = select, removeEmpty = TRUE)})
  expect_warning({m2C <- ModelMatrix(z, formula = ~age * geo * year, crossTable = TRUE, select = select, removeEmpty = TRUE)})
  expect_warning({m3C <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), formula = ~age * geo + year, crossTable = TRUE, select = select, removeEmpty = TRUE)})
  expect_warning({m1C_ <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), crossTable = TRUE, select = select, removeEmpty = FALSE)})
  expect_warning({m2C_ <- ModelMatrix(z, formula = ~age * geo * year, crossTable = TRUE, select = select, removeEmpty = FALSE)})
  expect_warning({m3C_ <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), formula = ~age * geo + year, crossTable = TRUE, select = select, removeEmpty = FALSE)})
  
  select <- selectD
  expect_warning({m1D <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), crossTable = TRUE, select = select, removeEmpty = TRUE)})
  expect_warning({m2D <- ModelMatrix(z, formula = ~age * geo * year, crossTable = TRUE, select = select, removeEmpty = TRUE)})
  expect_warning({m3D <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), formula = ~age * geo + year, crossTable = TRUE, select = select, removeEmpty = TRUE)})
  expect_warning({m1D_ <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), crossTable = TRUE, select = select, removeEmpty = FALSE)})
  expect_warning({m2D_ <- ModelMatrix(z, formula = ~age * geo * year, crossTable = TRUE, select = select, removeEmpty = FALSE)})
  expect_warning({m3D_ <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), formula = ~age * geo + year, crossTable = TRUE, select = select, removeEmpty = FALSE)})
  
  select <- selectE
  expect_warning({m1E <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), crossTable = TRUE, select = select, removeEmpty = TRUE)})
  expect_warning({m2E <- ModelMatrix(z, formula = ~age * geo * year, crossTable = TRUE, select = select, removeEmpty = TRUE)})
  expect_warning({m3E <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), formula = ~age * geo + year, crossTable = TRUE, select = select, removeEmpty = TRUE)})
  expect_warning({m1E_ <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), crossTable = TRUE, select = select, removeEmpty = FALSE)})
  expect_warning({m2E_ <- ModelMatrix(z, formula = ~age * geo * year, crossTable = TRUE, select = select, removeEmpty = FALSE)})
  expect_warning({m3E_ <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), formula = ~age * geo + year, crossTable = TRUE, select = select, removeEmpty = FALSE)})
  
  expect_equal(m1A, m1B)
  expect_equal(m2A, m2B)
  expect_equal(m3A, m3B)
  expect_equal(m1A, m1C)
  expect_equal(m2A, m2C)
  expect_equal(m3A, m3C)
  
  expect_true(all.equal(m1A_, m1C_, check.attributes = FALSE))
  expect_true(all.equal(m2A_, m2C_, check.attributes = FALSE))
  expect_true(all.equal(m3A_, m3C_, check.attributes = FALSE))
  
  expect_true(all.equal(selectB, m1B_$crossTable[names(selectB)], check.attributes = FALSE)) 
  expect_true(all.equal(selectB, m2B_$crossTable[names(selectB)], check.attributes = FALSE))
  expect_true(all.equal(selectB, m3B_$crossTable[names(selectB)], check.attributes = FALSE))
  
  m1AF <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), crossTable = FALSE, select = selectA, removeEmpty = TRUE)
  m2BF <- ModelMatrix(z, formula = ~age * geo * year, crossTable = FALSE, select = selectB, removeEmpty = TRUE)
  expect_warning({m3CF_ <- ModelMatrix(z, list(age = ageHier, geo = geoDimList, year = ""), formula = ~age * geo + year, crossTable = FALSE, select = selectC, removeEmpty = FALSE)})
  expect_equal(m1AF, m1A$modelMatrix)
  expect_equal(m2BF, m2B$modelMatrix)
  expect_equal(m3CF_, m3C_$modelMatrix)
  
  expect_true(all.equal(m1C$crossTable, m1C_$crossTable[colSums(m1C_$modelMatrix)!=0, , drop=FALSE], check.attributes = FALSE)) 
  expect_true(all.equal(m2C$crossTable, m2C_$crossTable[colSums(m2C_$modelMatrix)!=0, , drop=FALSE], check.attributes = FALSE)) 
  expect_true(all.equal(m3C$crossTable, m3C_$crossTable[colSums(m3C_$modelMatrix)!=0, , drop=FALSE], check.attributes = FALSE)) 
  
  expect_equal(m2D, m2D_)
  expect_equal(m3D, m3D_)
  expect_equal(m2E, m2E_)
  expect_equal(m3E, m3E_)
  expect_true(all.equal(m1D$crossTable, m1D_$crossTable[colSums(m1D_$modelMatrix)!=0, , drop=FALSE], check.attributes = FALSE)) 
  expect_true(all.equal(m1E$crossTable, m1E_$crossTable[colSums(m1E_$modelMatrix)!=0, , drop=FALSE], check.attributes = FALSE)) 
  
})  