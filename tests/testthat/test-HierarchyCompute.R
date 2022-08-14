context("HierarchyCompute")


# Data and hierarchies used in the examples
x <- SSBtoolsData("sprt_emp")  # Employment in sport in thousand persons from Eurostat database
geoHier <- SSBtoolsData("sprt_emp_geoHier")
ageHier <- SSBtoolsData("sprt_emp_ageHier")

# Make data and hierarchy more dirty
geoHier <- geoHier[c(1, 1:6), ]
geoHier[1, 1] <- "Italy"
geoHier[1, 2] <- "EU"
geoHier[2, 1] <- "Europe"
geoHier[2, 2] <- "World"
x[1, ] <- 0
x[18, 4] <- 0
x$y <- as.integer(10 * x$ths_per)
x$y2 <- 10*x$y
y2 <- c("y2", "y")

# Extend the hierarchy table to illustrate the effect of unionComplement 
# Omit level since this is handled by autoLevel
geoHier2 <- rbind(data.frame(mapsFrom = c("EU", "Spain"), mapsTo = "EUandSpain", sign = 1), 
                  geoHier[, -4])


test_that("handleDuplicated and asInput", {
  
  
  # Two hierarchies and year as rowFactor
  a = HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE)
  b = HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE)
  a_ = HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "rowFactor"), c("y", "ths_per"), verbose = FALSE)
  b_ = HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "colFactor"), c("y", "ths_per"), verbose = FALSE)
  expect_equal(a, b[, names(a)])
  expect_equal(a, a_[, names(a)])
  expect_equal(a_, b_[, names(a_)])
  
  for(handleDuplicated  in c("sumByAggregate", "sumWithWarning", "stop", "single", "singleWithWarning")){
    expect_equal(a, HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE,
                                   handleDuplicated = handleDuplicated))
    expect_equal(b, HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE,
                                     handleDuplicated = handleDuplicated))
    expect_equal(a_, HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "rowFactor"), c("y", "ths_per"), verbose = FALSE,
                                     handleDuplicated = handleDuplicated))
    expect_equal(b_, HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = "colFactor"), c("y", "ths_per"), verbose = FALSE,
                                     handleDuplicated = handleDuplicated))
  }
    
  x2 <- rbind(x[x$geo == "Iceland", ], x)
  a2 = HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE)
  b2 = HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE)
  a2_ = HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), c("y", "ths_per"), verbose = FALSE)
  b2_ = HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), c("y", "ths_per"), verbose = FALSE)
  expect_equal(a2, b2[, names(a2)])
  expect_equal(a2, a2_[, names(a2)])
  expect_equal(a2_, b2_[, names(a2_)])
  
  expect_equal(a2, HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE,
                                    handleDuplicated = "sumByAggregate"))
  
  expect_equal(b2, HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE,
                                    handleDuplicated = "sumByAggregate"))
  
  expect_equal(a2_, HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), c("y", "ths_per"), verbose = FALSE,
                                    handleDuplicated = "sumByAggregate"))
  
  expect_equal(b2_, HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), c("y", "ths_per"), verbose = FALSE,
                                    handleDuplicated = "sumByAggregate"))
  
  
  
  expect_warning({a2_sumWithWarning <- HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE,
                                   handleDuplicated = "sumWithWarning")})
  
  expect_warning({b2_sumWithWarning <- HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE,
                                                        handleDuplicated = "sumWithWarning")})
  
  expect_warning({a2_sumWithWarning_ <- HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), c("y", "ths_per"), verbose = FALSE,
                                                        handleDuplicated = "sumWithWarning")})
  
  expect_warning({b2_sumWithWarning_ <- HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), c("y", "ths_per"), verbose = FALSE,
                                                        handleDuplicated = "sumWithWarning")})
  
  expect_equal(a2, a2_sumWithWarning)
  expect_equal(b2, b2_sumWithWarning)
  expect_equal(a2_, a2_sumWithWarning_)
  expect_equal(b2_, b2_sumWithWarning_)
  
  
  expect_error(HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE,
                                                        handleDuplicated = "stop"))
  
  expect_error(HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE,
                                                        handleDuplicated = "stop"))
  
  
  expect_error(HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), c("y", "ths_per"), verbose = FALSE,
                                handleDuplicated = "stop"))
  
  expect_error(HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), c("y", "ths_per"), verbose = FALSE,
                                handleDuplicated = "stop"))
  
  
  expect_equal(a, HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE,
                                    handleDuplicated = "single"))
  
  expect_equal(b, HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE,
                                    handleDuplicated = "single"))
  
  expect_equal(a_, HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), c("y", "ths_per"), verbose = FALSE,
                                   handleDuplicated = "single"))
  
  expect_equal(b_, HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), c("y", "ths_per"), verbose = FALSE,
                                   handleDuplicated = "single"))
  
  
  expect_warning({a2_singleWithWarning <- HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE,
                                                        handleDuplicated = "singleWithWarning")})
  
  expect_warning({b2_singleWithWarning <- HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), "ths_per", verbose = FALSE,
                                                        handleDuplicated = "singleWithWarning")})
  
  expect_warning({a2_singleWithWarning_ <- HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), c("y", "ths_per"), verbose = FALSE,
                                                           handleDuplicated = "singleWithWarning")})
  
  expect_warning({b2_singleWithWarning_ <- HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "colFactor"), c("y", "ths_per"), verbose = FALSE,
                                                           handleDuplicated = "singleWithWarning")})
  
  expect_equal(a, a2_singleWithWarning)
  expect_equal(b, b2_singleWithWarning)
  expect_equal(a_, a2_singleWithWarning_)
  expect_equal(b_, b2_singleWithWarning_)
  
  
  valueMatrix = HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), "ths_per", verbose = FALSE, output = "valueMatrix", asInput = TRUE)
  valueMatrix_ = HierarchyCompute(x2, list(age = ageHier, geo = geoHier2, year = "rowFactor"), c("y", "ths_per"), verbose = FALSE, output = "valueMatrix", asInput = TRUE)
  
  expect_equivalent(matrix(x2[, "ths_per", drop = TRUE],ncol=1), as.matrix(valueMatrix))
  expect_equivalent(as.matrix(x2[, c("y", "ths_per")]),as.matrix(valueMatrix_))
  
  
})

test_that("parameter reOrder", {
  
  ExpectOK <- function(a, b, a_, b_) {
    expect_identical(a$valueMatrix, b$valueMatrix)
    expect_identical(a_$valueMatrix, b_$valueMatrix)
    expect_identical(a$valueMatrix, a$valueMatrix[ ,seq_len(ncol(a$valueMatrix)) ,drop=FALSE])
    expect_identical(a$fromCrossCode, b$fromCrossCode)
    expect_identical(a_$fromCrossCode, b_$fromCrossCode)
    expect_identical(a$fromCrossCode, a_$fromCrossCode)
    expect_identical(a$dataDummyHierarchy,a_$dataDummyHierarchy)
    expect_identical(b$dataDummyHierarchy,b_$dataDummyHierarchy)
    expect_identical(a$toCrossCode,a_$toCrossCode)
    expect_identical(b$toCrossCode,b_$toCrossCode)
    r <- match(rownames(a$dataDummyHierarchy), rownames(b$dataDummyHierarchy))
    expect_identical(as.matrix(a$dataDummyHierarchy), as.matrix(b$dataDummyHierarchy[r, , drop = FALSE]))
    expect_equivalent(a$toCrossCode, b$toCrossCode[r, , drop = FALSE])
  }
  
  for (yearFactor in c("rowFactor", "colFactor")) for (unionComplement in c("FALSE", "TRUE")) {
    a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = yearFactor), "ths_per", output = "matrixComponents", 
                          reOrder = FALSE, inputInOutput = TRUE, unionComplement = unionComplement)
    b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = yearFactor), "ths_per", output = "matrixComponents", 
                          reOrder = TRUE, inputInOutput = TRUE, unionComplement = unionComplement)
    a_ <- HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = yearFactor), c("ths_per", "y"), output = "matrixComponents", 
                          reOrder = FALSE, inputInOutput = TRUE, unionComplement = unionComplement)
    b_ <- HierarchyCompute(x, list(age = ageHier, geo = geoHier2, year = yearFactor), c("ths_per", "y"), output = "matrixComponents", 
                          reOrder = TRUE, inputInOutput = TRUE, unionComplement = unionComplement)
    ExpectOK(a, b, a_, b_)
  }
  
})



test_that("miscellaneous", {
  
  x <- rbind(x[x$geo == "Iceland", ], x)
  
  a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", inputInOutput = TRUE, reduceData = FALSE)
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "y", inputInOutput = TRUE, reduceData = FALSE)
  expect_identical(a, b[, names(a)])
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), y2, inputInOutput = TRUE, reduceData = FALSE)
  expect_identical(a, b[, names(a)])
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), y2, inputInOutput = TRUE, reduceData = FALSE)
  expect_identical(a, b[, names(a)])
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "y", inputInOutput = TRUE, reduceData = TRUE)
  expect_identical(a, b[, names(a)])
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), y2, inputInOutput = TRUE, reduceData = TRUE)
  expect_identical(a, b[, names(a)])
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", inputInOutput = TRUE, reduceData = TRUE)
  expect_identical(a, b[, names(a)])
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), y2, inputInOutput = TRUE, reduceData = TRUE)
  expect_identical(a, b[, names(a)])
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", inputInOutput = FALSE)
  expect_equivalent(a[Match(b, a), names(b)], b)
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), y2, inputInOutput = FALSE)[,names(a)]
  expect_equivalent(a[Match(b, a), names(b)], b)
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", inputInOutput = c(FALSE, TRUE))
  expect_equivalent(a[Match(b, a), names(b)], b)
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), y2, inputInOutput = c(FALSE, TRUE))[,names(a)]
  expect_equivalent(a[Match(b, a), names(b)], b)
  
  for (i in 1:5) {
    if (i == 1) rows <- c(11 * (1:13), 8, 19)
    if (i == 2) rows <- 100
    if (i == 3) rows <- 104
    if (i == 4) rows <- integer(0)
    if (i == 5) rows <- c(8, 11, 19, 100, 104, 8, 11)
    
    rowSelect <- a[rows, 1:3, drop = FALSE]
    
    d <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "y", rowSelect = rowSelect, selectionByMultiplicationLimit = 0)
    expect_equivalent(a[unique(rows), ], d[, names(a)])
    b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "y", rowSelect = rowSelect, inputInOutput = TRUE)
    expect_identical(b, d)
    b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "y", reduceData = FALSE, rowSelect = rowSelect, selectionByMultiplicationLimit = 0)
    expect_equivalent(d, b[, names(d)])
    b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), y2, reduceData = TRUE, rowSelect = rowSelect, selectionByMultiplicationLimit = 0)
    expect_equivalent(d, b[, names(d)])
    b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), y2, reduceData = FALSE, rowSelect = rowSelect)
    expect_equivalent(d, b[, names(d)])
  }
  
  for (i in 1:4) {
    if (i == 1) rows <- 11 * 4:12
    if (i == 2) rows <- 77
    if (i == 3) rows <- 88
    if (i == 4) rows <- integer(0)
    rowSelect <- a[rows, 2:3, drop = FALSE]
    for (j in c(2, 3, 4)) {   # test-HierarchyCompute problem seen here ... j=1 temporarily omitted due to  development version of Matrix 1.4-2 # for (j in 1:4) {
      if (j == 1) d <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", rowSelect = rowSelect, selectionByMultiplicationLimit = 0)
      if (j == 2) d <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), y2, rowSelect = rowSelect, selectionByMultiplicationLimit = 0, reduceData = FALSE)
      if (j == 3) d <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", rowSelect = rowSelect, reduceData = FALSE)
      if (j == 4) d <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), y2, rowSelect = rowSelect, reduceData = TRUE)
      ma <- Match(a[, 2:3], rowSelect)
      if (i == 4) 
        expect_false(any(!is.na(ma))) else expect_identical(diff(range(diff(sort(Match(a[which(!is.na(ma)), ], d[, names(a)]))))), 0L)
    }
    
  }
  
  
  a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "y", inputInOutput = TRUE, reduceData = FALSE, reOrder = TRUE, output = "matrixComponents")
  b <- Hierarchies2ModelMatrix(x, list(age = ageHier, geo = geoHier, year = "rowFactor"))
  expect_identical(a$dataDummyHierarchy, t(b))
  a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), y2, inputInOutput = TRUE, reduceData = FALSE, reOrder = TRUE, output = "matrixComponents")
  expect_identical(a$dataDummyHierarchy, t(b))
  
  z <- SSBtoolsData("sprt_emp")
  ageGeoYearFactor <- list(age = "", geo = "", year = "")
  
  m1 <- HierarchiesAndFormula2ModelMatrix(z, ageGeoYearFactor, ~geo * age + year:geo)
  m2 <- Formula2ModelMatrix(z, ~geo * age + year:geo)
  expect_identical(t(m1), t(m2))  # t is trick that removes new attributes 
  
  m1 <- HierarchiesAndFormula2ModelMatrix(x, ageGeoYearFactor, ~geo * age + year:geo)
  m2 <- Formula2ModelMatrix(x, ~geo * age + year:geo)
  m3 <- HierarchiesAndFormula2ModelMatrix(x, ageGeoYearFactor, ~geo * age + year:geo, removeEmpty = TRUE)
  m1 = m1[ ,colSums(abs(m1))!=0]
  expect_identical(t(m1), t(m2))
  expect_identical(t(m1), t(m3))
  
  m1 <- Hierarchies2ModelMatrix(x, ageGeoYearFactor, select = "removeEmpty")
  m2 <- Formula2ModelMatrix(x, ~age:geo:year - 1, sep=":")
  m3 <- Hierarchies2ModelMatrix(x, ageGeoYearFactor, removeEmpty = TRUE)
  expect_identical(t(m1), t(m2))
  expect_identical(t(m1), t(m3))
  
  ageGeoYearTotal <- list(age = "Total", geo = "Total", year = "Total")
  m1 <- Hierarchies2ModelMatrix(x, ageGeoYearTotal, select = "removeEmpty")
  m2 <- Formula2ModelMatrix(x, ~age*geo*year, sep=":")
  expect_identical(m1, m2[, match(colnames(m1),colnames(m2))])
  
  
  
  # -- New test based on earlier bug --
  # Create some input
  z <- SSBtoolsData("sprt_emp_withEU")
  z$age[z$age=="Y15-29"] <- "young"
  z$age[z$age=="Y30-64"] <- "old" 
  ageHier <- data.frame(mapsFrom = c("young", "old"), mapsTo = "Total", sign = 1)
  geoDimList <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
  # Small dataset example. Two dimensions.
  s <- z[z$geo == "Spain" & z$year != 2016, ]
  m1 <- HierarchiesAndFormula2ModelMatrix(s, list(age = ageHier, geo = geoDimList, year = ""), formula = ~age*geo + year, inputInOutput = c(TRUE, FALSE), removeEmpty = FALSE, crossTable = TRUE)$crossTable
  m2 <- HierarchiesAndFormula2ModelMatrix(s, list(age = ageHier, geo = geoDimList, year = ""), formula = ~year + age*geo, inputInOutput = c(TRUE, FALSE), removeEmpty = FALSE, crossTable = TRUE)$crossTable
  expect_identical(unique(diff(sort(Match(m1,m2)))),1L)
  
})



ReB = function(a,b){
  if(nrow(a) != nrow(b))
    stop("nrow error")
  ma = Match(a,b[,names(a)])
  z = b[ma,names(a)]
  rownames(z) = NULL
  z
}



test_that("HierarchyCompute2", {
  
  x <- rbind(x[x$geo == "Iceland", ], x)
  
  # HierarchyCompute2 inputInOutput = c(FALSE, TRUE)
  a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", inputInOutput = c(FALSE, TRUE), reduceData = FALSE)
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", "year", inputInOutput = c(FALSE, TRUE))
  expect_identical(a, b)
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", "geo",inputInOutput = c(FALSE, TRUE))
  expect_identical(a, ReB(a,b))
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", c("geo","year"), inputInOutput = c(FALSE, TRUE))
  expect_identical(a, ReB(a,b))
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), y2, c("year","age"), inputInOutput = c(FALSE, TRUE))
  expect_identical(a, ReB(a,b))
  
  
  # HierarchyCompute2 inputInOutput = TRUE
  a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", inputInOutput = TRUE, reduceData = FALSE)
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", "year", inputInOutput = TRUE)
  expect_identical(a, b)
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", "geo", inputInOutput = TRUE)
  expect_identical(a, ReB(a,b))
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", c("geo","year"), inputInOutput = TRUE)
  expect_identical(a, ReB(a,b))
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), y2, c("year","age"), inputInOutput = TRUE)
  expect_identical(a, ReB(a,b))
  
  # With handleDuplicated = "single"
  d <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", inputInOutput = TRUE, handleDuplicated = "single")
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", "year", inputInOutput = TRUE, handleDuplicated = "single")
  expect_identical(d, b)
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", "geo", inputInOutput = TRUE,  handleDuplicated = "single")
  expect_identical(d, ReB(d,b))
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), y2, c("geo","year"), inputInOutput = TRUE, handleDuplicated = "single")
  expect_identical(d, ReB(d,b))
  b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", c("year","age"), inputInOutput = TRUE, handleDuplicated = "single")
  expect_identical(d, ReB(d,b))
  
  
  # With select
  aS_ = a[round((1:20)*7.2), ]
  aS_ = aS_[aS_$year != "2015", ]
  for (i in c(1, 2, 3, 5)) {    # test-HierarchyCompute problem seen here ... i=4 temporarily omitted due to  development version of Matrix 1.4-2 # for (i in 1:5) {    
    if (i == 1) rows <- 1:15
    if (i == 2) rows <- 1:3
    if (i == 3) rows <- 4
    if (i == 4) rows <- 6
    if (i == 5) rows <- integer(0)
    
    aS = aS_[rows, ,drop=FALSE]
    row.names(aS) = NULL
    b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), y2, inputInOutput = TRUE, select = aS[, 1:3])
    expect_identical(aS, b[, names(aS)])
    b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "y", inputInOutput = TRUE, select = aS[, 1:3])
    expect_identical(aS, b[, names(aS)])
    b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), y2, c("geo","year"), inputInOutput = TRUE, select = aS[, 1:3])
    expect_identical(aS, b[, names(aS)])
    b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), y2, "geo", inputInOutput = TRUE, select = aS[, 1:3])
    expect_identical(aS, b[, names(aS)])
    b <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "y", c("year","age"), inputInOutput = TRUE, select = aS[, 1:3])
    expect_identical(aS, b[, names(aS)])
  }
  
})

















