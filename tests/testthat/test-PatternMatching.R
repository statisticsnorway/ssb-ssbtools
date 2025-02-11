context("PatternMatching")

test_that("HierarchicalWildcardGlobbing and WildcardGlobbingVector", {
  
  data(precip)
  data(mtcars)
  codes <- as.character(c(100, 200, 300, 600, 700, 101, 102, 103, 104, 134, 647, 783,
                          13401, 13402, 64701, 64702))
  
  # Create list input
  zList <- list(car = rownames(mtcars), wt = as.character(1000 * mtcars$wt),
                city = names(precip), code = codes)
  
  # Create data.frame input
  m <- cbind(car = rownames(mtcars), wt = as.character(1000 * mtcars$wt))
  zFrame <- data.frame(m[rep(1:NROW(m), each = 35), ],
                       city = names(precip), code = codes, stringsAsFactors = FALSE)
  
  # Create globbing/wildcards input
  wg <- data.frame(rbind(c("Merc*", ""    , ""    , "?00"  ),
                         c("F*"   , ""    , ""    , "?????"),
                         c(""     , "???0", "C*"  , ""     ),
                         c(""     , ""    , "!Co*", ""     ),
                         c(""     , ""    , "?i*" , "????2"),
                         c(""     , ""    , "?h*" , "????1")),
                   sign = c("+", "+", "+", "+", "-", "-"), stringsAsFactors = FALSE)
  names(wg)[1:4] <- names(zList)
  
  expect_equal(dim(HierarchicalWildcardGlobbing(zList, wg)), c(4788, 4))
  
  hg <- HierarchicalWildcardGlobbing(zFrame, wg)
  expect_equal(dim(hg), c(9, 4))
  wg[4,3] <- ".Co*"
  expect_equal(hg, HierarchicalWildcardGlobbing(zFrame, wg, invert = "."))
  
  expect_equal(dim(HierarchicalWildcardGlobbing(as.list(zFrame), wg, invert = ".")), c(4788, 4))
  
  x <- names(precip)
  
  expect_equal(
    WildcardGlobbingVector(x, c("B*", "C*", "Sa*", "-?o*", "-???t*", "!??????*")),
    WildcardGlobbingVector(x, c("B*", "C*", "Sa*", "!?o*", "!???t*", "-??????*"),
                           negSign = "!", invert = "-"))
  
})
