test_that("formula_utils", {
  f1 <- ~b + a*c + b:d
  f2 <- substitute_formula_terms(f1, list(a = c("hello", "world", "b"), 
                                          b = c("Q1", "Q2"),
                                          c = "C"))
  f2c <- as.character(f2)
  expect_equal(f2c[length(f2c)], 
               "(Q1 + Q2) + (hello + world + b) * C + (Q1 + Q2):d")
})
