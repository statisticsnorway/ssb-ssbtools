
#' Weighted quantiles
#' 
#' The default method (`type=2`) corresponds to weighted percentiles in SAS. 
#' 
#' @details
#' When `type=2`, averaging is used in case of equal of probabilities. 
#' Equal probabilities (`p[j]==probs[i]`) is determined by 
#' `abs(1-p[j]/probs[i])<eps` 
#' with `p=cumsum(w)/sum(w)`
#' where `w=weights[order(x)]`.
#' 
#' With zero length of `x`, `NA`s are returned.
#' 
#' When all weights are zero and when when all `x`'s are not equal, 
#' `NaN`s are returned except for the 0% and 100% quantiles.
#'
#' @param x 	Numeric vector 
#' @param probs Numeric vector of probabilities
#' @param weights Numeric vector of weights of the same length as `x`
#' @param type An integer, `2` (default) or `5`. Similar to types 2 and 5 in \code{\link{quantile}}.
#' @param eps Precision parameter used when `type=2` so that numerical inaccuracy is accepted (see details)
#'
#' @return Quantiles as a named numeric vector. 
#' @export
#' @importFrom stats approx
#' 
#' @note Type 2 similar to type 5 in `DescTools::Quantile`
#'
#' @examples
#' x <- rnorm(27)/5 + 1:27
#' w <- (1:27)/27
#' 
#' quantile_weighted(x, (0:5)/5, weights = w)
#' quantile_weighted(x, (0:5)/5, weights = w, type = 5)
#' 
#' quantile_weighted(x) - quantile(x, type = 2)
#' quantile_weighted(x, type = 5) - quantile(x, type = 5)
#' 
quantile_weighted <- function(x, probs = (0:4)/4, weights = rep(1, length(x)), type = 2, eps = 1e-09) {
  
  if (!(type %in% c(2, 5))) {
    stop("type must be 2 or 5")
  }
  
  empty <- (length(x) == 0)
  if (empty) {
    x <- 0
    weights <- 1
  }
  
  n <- length(probs)
  length_x <- length(x)
  
  ox <- order(x)
  x <- x[ox]
  
  if(x[1] == x[length_x]){   # All zero weights combinded with all equal x included here 
    x <- x[1]
    w <- 1
    length_x <- 1L
  } else {
    w <- as.numeric(weights[ox])
  }
  
  weights0 <- FALSE
  if (!w[1]) if (!max(w)) if (!min(w)) {  # All zero weights combinded with NOT all equal x -> NA's 
    weights0 <- TRUE
    x <- x[c(1, length_x)]
    w <- c(1, 1)
    length_x <- 2L
  }
  
  p <- cumsum(w)/sum(w)
  
  if (type == 2) {
    out <- rep(x[1] + NA, n)
    for (i in seq_len(n)) {
      if (probs[i] == 0) {
        out[i] <- x[1]
      } else {
        if (probs[i] == 1) {
          out[i] <- x[length_x]
        } else {
          ind <- which(p >= probs[i])
          if (length(ind)) {
            ind <- min(ind)
          } else {
            ind <- length_x
          }
          if (abs(1 - p[ind]/probs[i]) < eps) {
            if (ind < length_x) {
              out[i] <- mean(x[ind:(ind + 1)])
            } else {
              out[i] <- x[ind]
            }
          } else {
            if (ind == 1) {
              out[i] <- x[ind]
            } else {
              if (abs(1 - p[ind - 1]/probs[i]) < eps) {
                out[i] <- mean(x[(ind - 1):ind])
              } else {
                out[i] <- x[ind]
              }
            }
          }
        }
      }
    }
  } else {  # type == 5
    p2 <- 1 - rev(cumsum(rev(w))/sum(w))
    pw <- (p + p2)/2
    pw <- c(0, pw, 1)
    x <- c(x[1], x, x[length(x)])
    out <- approx(pw, x, xout = probs)$y
  }
  
  if (empty)
    out <- out + NA
  
  if (weights0)
    out[!(probs %in% c(0, 1))] <- NaN
  
  names(out) <- paste0(round(100 * probs, 5), "%")
  out
}


