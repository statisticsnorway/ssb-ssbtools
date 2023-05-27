


# Kostra:::HeadEnd
# HeadEnd(1:1000) '1' '2' '3' '4' '...'  '1000'
HeadEnd <- function(x, n = 4L) {
  x <- as.character(x)
  if (length(x) > (n + 2))
    x <- c(head(x, n = n), "...", tail(x, n = 1))
  x
}


