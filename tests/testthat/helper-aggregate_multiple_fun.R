
# Had to be included in helper file to work


ff <- function(a1 = 0, a2 = 0, a3 = 0, ...) {
  f5(a1, a2, a3, ...)
}

f5 <- function(a1 = 0, a2 = 0, a3 = 0, a4 = 0, a5 = 0) {
  sum(a1 + 2 * a2 + 3 * a3 + 4 * a4 + 5 * a5)
}

zz <- data.frame(nr = c(1:2, 1:2), c1 = 1, c2 = 2, c3 = 3, c4 = 4)


amf <- function(...) {
  as.vector(as.matrix(aggregate_multiple_fun(
    data = zz, 
    by = zz["nr"], 
    fun = c(ff = function(a1 = 0, a2 = 0, a3 = 0, ...) {f5(a1, a2, a3, ...)}, f5 = f5), # Error within check if ff is not explicit
    vars = list(ff = "c1", f5 = c("c1", "c2", "c3"), f5 = c("c1", "c2", "c4")),
    ...)[2, 2:4]))
}



########

za <- SSBtoolsData("sprt_emp_withEU")
za$age[za$age == "Y15-29"] <- "young"
za$age[za$age == "Y30-64"] <- "old"
names(za)[names(za) == "ths_per"] <- "ths"
za$y <- 1:18 + round(1/9, 5)

my_range2 <- function(x, do_round = FALSE, mdigits = 0, ...) {
  minx <- min(x)
  maxx <- max(x)
  if (do_round) {
    minx <- round(minx, digits = mdigits)
    maxx <- round(maxx, ...)
  }
  c(min = minx, max = maxx)
}


ma <- function(..., dim_var = NULL, formula = ~age:year + geo, frame_return = FALSE) {
  a <- model_aggregate(za, dim_var = dim_var, formula = formula, 
                       sum_vars = c("y", "ths"), 
                       fun_vars = c(sum = "ths", mean = "y", ra = "y"), fun = c(sum = sum, mean = mean, ra = my_range2), 
                       verbose = FALSE, ...)
  if (frame_return) {
    return(a)
  }
  
  b <- a[a$age == "old" & a$year == "2016", c("y", "ths", "ths_sum", "y_mean", "y_ra.min", "y_ra.max")]
  
  as.vector(as.matrix(b))
}
