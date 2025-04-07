


AnyProportionalGaussInt_OLD <- function(r, x, rB, xB, tolGauss,  kk_2_factorsB) {
  n <- length(r)
  if(!n){
    return(TRUE) # Empty "A-input" regarded as proportional
  }
  for (i in seq_along(rB)) {
    ni <- length(xB[[i]])
    if (ni) {    # Empty "B-input" not regarded as proportional
      if (ni == n) {
        if (identical(r, rB[[i]])) {
          if (n==1L)
            return(TRUE)
          if (identical(x, xB[[i]])) 
            return(TRUE)
          if (identical(-x, xB[[i]])) 
            return(TRUE)
          
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
              return(TRUE)
            if(is.numeric(kk)){
              if( all(abs( xB[[i]] - kk_2_x/kk[1]) < tolGauss))
                return(TRUE)
            }
          }
          else {
            #if (FALSE) {
            #
            #  Possible code here to look at distribution of numeric computing errors  
            #
            #  aabb <- abs((xB[[i]] - (cx1xBi1[2]/cx1xBi1[1]) * x)/kk_2_factorsB[i])
            #  aabb <- aabb[aabb > 0 & aabb < 1e-04]
            #}
            if( all(abs(  xB[[i]] - (cx1xBi1[2]/cx1xBi1[1])* x) < tolGauss*abs(kk_2_factorsB[i]) )  )
              return(TRUE)
          }
        }
      }
    }
  }
  FALSE
}
