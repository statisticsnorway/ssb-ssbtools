
#' varGroups-attribute to Extend0, Example functions 
#' 
#' Setting `attr(varGroups, "FunctionExtend0")` to a function
#' makes `Extend0` behave differently
#' 
#' The point is to create a function that takes `data` and `varGroups` as input
#' and that returns a data frame with a limited number of combinations of the elements in `varGroups`.
#' The examples here use two `varGroups` elements, but any number of elements can be used.
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
  n <- k * nrow(data)
  nGroups <- sapply(varGroups, nrow)
  
  nrep <- ceiling(n / nGroups[1])
  N <- nGroups[1] * nrep
  
  ind <- matrix(NA_integer_, N, length(varGroups))
  ind[, 1] <- rep(seq_len(nGroups[1]), nrep)
  
  for (j in SeqInc(2, length(varGroups))) {
    nj <- nGroups[j]
    njrep <- floor(N / nj)
    
    ind[, j] <- sample(c(
      rep(seq_len(nj), njrep),
      sample.int(nj, N - nj * njrep)
    ))
  }
  
  ind <- SortRows(unique(ind))
  
  do.call(cbind, Map(
    function(x, i) x[i, , drop = FALSE],
    varGroups,
    as.data.frame(ind)
  ))
}

#' @rdname Extend0rnd1
#' @export
Extend0rnd2 <- function(...) Extend0rnd1(..., k = 2)


#' @rdname Extend0rnd1
#' @export
Extend0rnd1b <- function(...) Extend0rnd1(..., k = 1, rndSeed = 1)




#'  Add zero-frequency rows using complete and sampled group combinations
#'
#' `Extend0_with_n_rnd_groups` is a function that calls `Extend0()` with
#' `Extend0_n_rnd_groups()` as the `varGroups` attribute.
#' The data are extended in the usual way based on some of the `varGroups`
#' elements. For the remaining elements, combinations are sampled.
#'
#' @inheritParams Extend0rnd1
#' @param n_rnd_groups The last `n_rnd_groups` elements of `varGroups` are
#'   used as the basis for sampling.
#' @param rnd_rep When `rnd_rep = 1` (the default), the usual `Extend0`
#'   extension is performed for the selected `varGroups` elements.
#'   When `rnd_rep > 1`, this result is replicated to allow more combinations
#'   to be sampled.
#'
#' @seealso [Extend0rnd1()]
#'
#' @return A data frame.
#' @export
#' 
Extend0_n_rnd_groups <- function(data, 
                                 varGroups, 
                                 n_rnd_groups = 0, 
                                 rnd_rep = 1,
                                 rndSeed = 123) {
  if (!is.null(rndSeed)) {
    if (!exists(".Random.seed"))
      if (runif(1) < 0)
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(rndSeed)
  }
  
  z <- varGroups[[1]]
  n_groups <- length(varGroups)
  if (n_rnd_groups >= n_groups) {
    n_rnd_groups <- n_groups - 1
  }
  
  for (i in SeqInc(2, n_groups - n_rnd_groups)) {
    z <- CrossCodeFrames(z, varGroups[[i]])
  }
  
  if (rnd_rep > 1) {
    z <- z[rep(seq_len(nrow(z)), times = rnd_rep), ]
  }
  
  for (i in SeqInc(n_groups - n_rnd_groups + 1, n_groups)) {
    
    N <- nrow(z)
    ni <- nrow(varGroups[[i]])
    nirep <- floor(N/ni)
    
    ind <- sample(c(rep(seq_len(ni), nirep), sample.int(ni, N - ni * nirep)))
    
    z <- cbind(z, varGroups[[i]][ind, , drop = FALSE])
  }
  z <- z[!duplicated(names(z))]
  if (rnd_rep > 1) {
    ma <- Match(z, z)
    z <- z[unique(ma[!is.na(ma)]), , drop = FALSE]
  }
  z
}


#' @rdname Extend0_n_rnd_groups
#' @export
#' @param non_rnd In `Extend0_with_n_rnd_groups`, the `varGroups` elements
#'   that should not be sampled are specified by name. In this case,
#'   `varGroups` must be a named list.
#'   
#' @examples
#' 
#' # Data to be extended 
#' d <- SSBtoolsData("barcelona2025")[c(3, 6, 12, 18), -5]
#' rownames(d) <- NULL
#' d$year <- 2025:2026
#' d$month <- c("January", rep("August", 3))
#' d
#' 
#' # varGroups as a named list
#' varGroups <- list(geo = c("country", "city"), 
#'                   age = "age", 
#'                   sex = "sex", 
#'                   time = c("month", "year"))
#' 
#' a0 <- Extend0(d, varGroups = varGroups)
#' dim(a0)  # all combinations, 48 rows
#'  
#' a1 <- Extend0_with_n_rnd_groups(d, varGroups = varGroups, non_rnd = c("time", "geo"))
#' a1
#' dim(unique(a1[c("month", "year", "country", "city")])) # all combinations of time and geo  
#' 
#' unique(a1[c("sex", "month", "year")])        # not all combinations
#' 
#' 
#' a2 <- Extend0_with_n_rnd_groups(d, varGroups = varGroups, non_rnd = c("sex", "time", "geo"))
#' dim(a2)
#' 
#' # all combinations of selected variables 
#' dim(unique(a2[c("sex", "month", "year", "country", "city")]))
#'  
#' # not all combinations of selected variables 
#' dim(unique(a2[c("age", "month", "year", "country", "city")]))  
#'  
#' 
#' # effect of rnd_rep
#' for (rnd_rep in c(1, 2, 3, 5, 10, 50)) 
#'    print(dim(Extend0_with_n_rnd_groups(d, 
#'            varGroups = varGroups, non_rnd = c("time", "geo"), rnd_rep = rnd_rep)))
#'    
Extend0_with_n_rnd_groups <- function(data, 
                                      varGroups, 
                                      non_rnd, 
                                      rnd_rep = 1,
                                      rndSeed = 123) {
  
  if (is.null(names(varGroups)) || anyNA(names(varGroups)) || any(names(varGroups) == ""))
    stop("All elements of varGroups must be named.")
  
  if (!all(non_rnd %in% names(varGroups)))
    stop("All elements of non_rnd must be names in varGroups.")
  
  nr <- names(varGroups) %in% non_rnd
  varGroups_ <- c(varGroups[nr], varGroups[!nr])
  n_rnd_g <- sum((!nr))
  attr(varGroups_, "FunctionExtend0") <- 
    function(...) Extend0_n_rnd_groups(..., 
                                       n_rnd_groups = n_rnd_g,
                                       rnd_rep = rnd_rep,
                                       rndSeed = rndSeed)
  Extend0(data, varGroups = varGroups_)
}

















