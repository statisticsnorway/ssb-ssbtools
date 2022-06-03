
#' varGroups-attribute to Extend0, Example functions 
#' 
#' Setting `attr(varGroups, "FunctionExtend0")` to a function
#' makes `Extend0` behave differently
#' 
#' The point is to create a function that takes `data` and `varGroups` as input
#' and that returns a data frame with a limited number of combinations of the elements in `varGroups`.
#' The example function here is limited to two varGroups elements.
#'
#' @param data data.frame within \code{\link{Extend0}}
#' @param varGroups argument to \code{\link{Extend0}}
#' @param k Number of rows generated is approx. `k*nrow(data)`
#' @param rndSeed Internal random seed to be used 
#' @param ... Extra unused parameters
#'
#' @return a data frame 
#' @export
#' 
#' @examples
#' z <- SSBtoolsData("sprt_emp_withEU")[c(1, 5, 8, 14), ]
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' 
#' varGroups <- list(c("year", "geo", "eu"), data.frame(age = c("middle", "old")))
#' Extend0(z, varGroups = varGroups)
#' 
#' attr(varGroups, "FunctionExtend0") <- Extend0rnd1
#' Extend0(z, varGroups = varGroups)
#' 
#' attr(varGroups, "FunctionExtend0") <- Extend0rnd1b
#' Extend0(z, varGroups = varGroups)
#' 
#' attr(varGroups, "FunctionExtend0") <- Extend0rnd2
#' Extend0(z, varGroups = varGroups)
#' 
#' # To see what's going on internally. Data used only via nrow 
#' varGroups <- list(data.frame(ab = rep(c("a", "b"), each = 4), abcd = c("a", "b", "c", "d")), 
#'                   data.frame(AB = rep(c("A", "B"), each = 3), ABC = c("A", "B", "C"))) 
#' a <- Extend0rnd1(data.frame(1:5), varGroups)
#' table(a[[1]], a[[2]])
#' table(a[[3]], a[[4]])
#' a <- Extend0rnd1b(data.frame(1:5), varGroups)
#' table(a[[1]], a[[2]])
#' table(a[[3]], a[[4]])
#' a <- Extend0rnd2(data.frame(1:5), varGroups[2:1])
#' table(a[[1]], a[[2]])
#' table(a[[3]], a[[4]])
#' a <- Extend0rnd1(data.frame(1:100), varGroups)
#' table(a[[1]], a[[2]]) # Maybe smaller numbers than expected since duplicates were removed
#' table(a[[3]], a[[4]])
Extend0rnd1 <- function(data, varGroups, k = 1, rndSeed = 123) {
  if (!is.null(rndSeed)) {
    if (!exists(".Random.seed"))
      if (runif(1) < 0)
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(rndSeed)
  }
  if (length(varGroups) != 2)
    stop("length(varGroups) must be 2")
  n <- k * nrow(data)
  n1 <- nrow(varGroups[[1]])
  n2 <- nrow(varGroups[[2]])
  
  n1rep <- ceiling(n/n1)
  n2rep <- floor(n1 * n1rep/n2)
  ind <- cbind(rep(seq_len(n1), n1rep), 
               sample(c(rep(seq_len(n2), n2rep), sample.int(n2, n1 * n1rep - n2 * n2rep)))) # sample(rep(seq_len(n2), n2rep), n1 * n1rep))
  ind <- SortRows(unique(ind))
  
  cbind(varGroups[[1]][ind[, 1], , drop = FALSE], varGroups[[2]][ind[, 2], , drop = FALSE])
}

#' @rdname Extend0rnd1
#' @export
Extend0rnd2 <- function(...) Extend0rnd1(..., k = 2)


#' @rdname Extend0rnd1
#' @export
Extend0rnd1b <- function(...) Extend0rnd1(..., k = 1, rndSeed = 1)






