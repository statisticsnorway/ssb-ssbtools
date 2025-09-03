

# Identifies rows or columns that contain only zeros.
# Memory usage is reduced by applying abs() checks only 
# to rows/columns whose total sum is already zero.
zero_col <- function(x, rows = FALSE, value = 0) {
  if (rows) {
    a <- rowSums(x) == value
    if (any(a)) {
      a[a] <- rowSums(abs(x[a, , drop = FALSE])) == value
    }
    return(a)
  }
  a <- colSums(x) == value
  if (any(a)) {
    a[a] <- colSums(abs(x[, a, drop = FALSE])) == value
  }
  a
}


single_col <- function(..., value = 1) {
  zero_col(..., value = value)
}