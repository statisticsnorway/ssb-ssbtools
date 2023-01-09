
# Had to be included in helper file to work


ff <- function(a1 = 0, a2 = 0, a3 = 0, ...) {
  f5(a1, a2, a3, ...)
}

f5 <- function(a1 = 0, a2 = 0, a3 = 0, a4 = 0, a5 = 0) {
  sum(a1 + 2 * a2 + 3 * a3 + 4 * a4 + 5 * a5)
}

zz <- data.frame(nr = c(1:2, 1:2), c1 = 1, c2 = 2, c3 = 3, c4 = 4)

amf <- function(...) {
  ff <- function(a1 = 0, a2 = 0, a3 = 0, ...) {
    f5(a1, a2, a3, ...)
  }
  as.vector(as.matrix(aggregate_multiple_fun(
    data = zz, 
    by = zz["nr"], 
    fun = c("ff", "f5"), 
    vars = list(ff = "c1", f5 = c("c1", "c2", "c3"), f5 = c("c1", "c2", "c4")),
    ...)[2, 2:4]))
}
