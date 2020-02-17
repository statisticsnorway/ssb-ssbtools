context("HierarchyCompute")




test_that("handleDuplicated and asInput", {
  
  # Data and hierarchies used in the examples
  x <- SSBtoolsData("sprt_emp")  # Employment in sport in thousand persons from Eurostat database
  geoHier <- SSBtoolsData("sprt_emp_geoHier")
  ageHier <- SSBtoolsData("sprt_emp_ageHier")
  
  # Extend the hierarchy table to illustrate the effect of unionComplement 
  # Omit level since this is handled by autoLevel
  geoHier2 <- rbind(data.frame(mapsFrom = c("EU", "Spain"), mapsTo = "EUandSpain", sign = 1), 
                    geoHier[, -4])
  
  # Two hierarchies and year as rowFactor
  a = HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE)
  b = HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE)
  expect_equal(a, b[, names(a)])
  
  for(handleDuplicated  in c("sumByAggregate", "sumWithWarning", "stop", "single", "singleWithWarning")){
    expect_equal(a, HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE,
                                   handleDuplicated = handleDuplicated))
    expect_equal(b, HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE,
                                     handleDuplicated = handleDuplicated))
  }
    
  x2 <- rbind(x[x$geo == "Iceland", ], x)
  a2 = HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE)
  b2 = HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE)
  expect_equal(a2, b2[, names(a2)])
  
  expect_equal(a2, HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE,
                                    handleDuplicated = "sumByAggregate"))
  
  expect_equal(b2, HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE,
                                    handleDuplicated = "sumByAggregate"))
  
  
  expect_warning({a2_sumWithWarning <- HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE,
                                   handleDuplicated = "sumWithWarning")})
  
  expect_warning({b2_sumWithWarning <- HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE,
                                                        handleDuplicated = "sumWithWarning")})
  
  expect_equal(a2, a2_sumWithWarning)
  expect_equal(b2, b2_sumWithWarning)
  
  
  expect_error(HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE,
                                                        handleDuplicated = "stop"))
  
  expect_error(HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE,
                                                        handleDuplicated = "stop"))
  
  
  expect_equal(a, HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE,
                                    handleDuplicated = "single"))
  
  expect_equal(b, HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE,
                                    handleDuplicated = "single"))
  
  
  expect_warning({a2_singleWithWarning <- HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE,
                                                        handleDuplicated = "singleWithWarning")})
  
  expect_warning({b2_singleWithWarning <- HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE,
                                                        handleDuplicated = "singleWithWarning")})
  
  expect_equal(a, a2_singleWithWarning)
  expect_equal(b, b2_singleWithWarning)
  
  
  
  valueMatrix = HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE, output = "valueMatrix", asInput = TRUE)
  
  expect_equivalent(matrix(x2[, "ths_per"],ncol=1), as.matrix(valueMatrix))
  
})

test_that("parameter reOrder", {
  
  # Data and hierarchies used in the examples
  x <- SSBtoolsData("sprt_emp")  # Employment in sport in thousand persons from Eurostat database
  geoHier <- SSBtoolsData("sprt_emp_geoHier")
  ageHier <- SSBtoolsData("sprt_emp_ageHier")
  
  # Extend the hierarchy table to illustrate the effect of unionComplement 
  # Omit level since this is handled by autoLevel
  geoHier2 <- rbind(data.frame(mapsFrom = c("EU", "Spain"), mapsTo = "EUandSpain", sign = 1), 
                    geoHier[, -4])
  
  ExpectOK <- function(a, b) {
    expect_identical(a$valueMatrix, b$valueMatrix)
    expect_identical(a$fromCrossCode, b$fromCrossCode)
    r <- match(rownames(a$dataDummyHierarchy), rownames(b$dataDummyHierarchy))
    expect_identical(as.matrix(a$dataDummyHierarchy), as.matrix(b$dataDummyHierarchy[r, , drop = FALSE]))
    expect_equivalent(a$toCrossCode, b$toCrossCode[r, , drop = FALSE])
  }
  
  for (yearFactor in c("rowFactor", "colFactor")) for (unionComplement in c("FALSE", "TRUE")) {
    a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = yearFactor), "ths_per", output = "matrixComponents", 
                          reOrder = FALSE, inputInOutput = TRUE, unionComplement = unionComplement)
    b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = yearFactor), "ths_per", output = "matrixComponents", 
                          reOrder = TRUE, inputInOutput = TRUE, unionComplement = unionComplement)
    ExpectOK(a, b)
  }
  
})



test_that("miscellaneous", {
  
  # Data and hierarchies used in the examples
  x <- SSBtoolsData("sprt_emp")  # Employment in sport in thousand persons from Eurostat database
  ageHier <- SSBtoolsData("sprt_emp_ageHier")
  geoHier <- SSBtoolsData("sprt_emp_geoHier")
  
  
  # Make data and hierarchy more dirty
  geoHier <- geoHier[c(1, 1:6), ]
  geoHier[1, 1] <- "Italy"
  geoHier[1, 2] <- "EU"
  geoHier[2, 1] <- "Europe"
  geoHier[2, 2] <- "World"
  x[1, ] <- 0
  x[18, 4] <- 0
  x$y <- as.integer(10 * x$ths_per)
  
  a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", inputInOutput = TRUE, reduceData = FALSE)
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "y", inputInOutput = TRUE, reduceData = FALSE)
  expect_identical(a, b[, names(a)])
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "y", inputInOutput = TRUE, reduceData = TRUE)
  expect_identical(a, b[, names(a)])
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", inputInOutput = TRUE, reduceData = TRUE)
  expect_identical(a, b[, names(a)])
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", inputInOutput = FALSE)
  expect_equivalent(a[Match(b, a), names(b)], b)
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", inputInOutput = c(FALSE, TRUE))
  expect_equivalent(a[Match(b, a), names(b)], b)
  
  
  a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "y", inputInOutput = TRUE, reduceData = FALSE, reOrder = TRUE, output = "matrixComponents")
  b <- Hierarchies2ModelMatrix(x, list(age = ageHier, geo = geoHier, year = "rowFactor"))
  expect_identical(a$dataDummyHierarchy, t(b))
  
  z <- SSBtoolsData("sprt_emp")
  ageGeoYearFactor <- list(age = "", geo = "", year = "")
  
  m1 <- HierarchiesAndFormula2ModelMatrix(z, ageGeoYearFactor, ~geo * age + year:geo)
  m2 <- Formula2ModelMatrix(z, ~geo * age + year:geo)
  expect_identical(t(m1), t(m2))  # t is trick that removes new attributes 
  
  m1 <- HierarchiesAndFormula2ModelMatrix(x, ageGeoYearFactor, ~geo * age + year:geo)
  m2 <- Formula2ModelMatrix(x, ~geo * age + year:geo)
  m1 = m1[ ,colSums(abs(m1))!=0]
  expect_identical(t(m1), t(m2))
  
  m1 <- Hierarchies2ModelMatrix(x, ageGeoYearFactor, select = "removeEmpty")
  m2 <- Formula2ModelMatrix(x, ~age:geo:year - 1, sep=":")
  expect_identical(m1, m2)
  
  ageGeoYearTotal <- list(age = "Total", geo = "Total", year = "Total")
  m1 <- Hierarchies2ModelMatrix(x, ageGeoYearTotal, select = "removeEmpty")
  m2 <- Formula2ModelMatrix(x, ~age*geo*year, sep=":")
  expect_identical(m1, m2[, match(colnames(m1),colnames(m2))])
  
})



















