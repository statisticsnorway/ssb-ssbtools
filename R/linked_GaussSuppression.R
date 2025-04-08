

# A version that not only checks if any cells are proportional,
# but returns whether each cell is proportional or not.
AnyProportionalGaussInt_OLD_ALL <- function(r, x, rB, xB, tolGauss,  kk_2_factorsB) {
  n <- length(r)
  if(!n){
    stop("Unforeseen problem")
  }
  out <- rep(FALSE, length(rB))
  for (i in seq_along(rB)) {
    ni <- length(xB[[i]])
    if (ni) {    # Empty "B-input" not regarded as proportional
      if (ni == n) {
        if (identical(r, rB[[i]])) {
          if (n==1L)
          {out[i] <- TRUE; next;}
          if (identical(x, xB[[i]])) 
          {out[i] <- TRUE; next;}
          if (identical(-x, xB[[i]])) 
          {out[i] <- TRUE; next;}
          
          cx1xBi1 <- c(x[1], xB[[i]][1])
          if(is.integer(cx1xBi1)){
            kk <- ReduceGreatestDivisor(cx1xBi1)
            suppressWarnings({
              kk_2_x <- kk[2] * x 
              kk_1_xB_i <- kk[1] * xB[[i]]
            })
            if(anyNA(kk_2_x) | anyNA(kk_1_xB_i)){
              kk <- as.numeric(kk)
              kk_2_x <- kk[2] * x 
              kk_1_xB_i <- kk[1] * xB[[i]]
              
            }   
            if (identical(kk_2_x, kk_1_xB_i)) 
            {out[i] <- TRUE; next;}
            if(is.numeric(kk)){
              if( all(abs( xB[[i]] - kk_2_x/kk[1]) < tolGauss))
              {out[i] <- TRUE; next;}
            }
          }
          else {
            if( all(abs(  xB[[i]] - (cx1xBi1[2]/cx1xBi1[1])* x) < tolGauss*abs(kk_2_factorsB[i]) )  )
            {out[i] <- TRUE; next;}
          }
        }
      }
    }
  }
  out
}
