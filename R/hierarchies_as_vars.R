


#' Hierarchies coded as variables 
#' 
#' The hierarchical relations are stored as minimal datasets
#'
#' @param hierarchies List of hierarchies in the same format as input to \code{\link{AutoHierarchies}}
#' @param name_function A function defining how to name all columns except the first.
#'                      The input consists of the hierarchy name (identical to the first column’s name, `name`)
#'                      and the column number minus 1 (`level`).
#' @param single_vars When `TRUE`, a single variable is created for all codes except the input codes.
#' @param from_dummy Logical value indicating the method for handling hierarchies.
#'   - When `TRUE`, the algorithm uses dummy-coded hierarchies.
#'   - When `FALSE`, the algorithm works directly on hierarchies standardized by `AutoHierarchies`, often resulting in well-structured output variables.
#'   - When `NA` (default), the algorithm first attempts the `FALSE` method; if not feasible, it falls back to the `TRUE` method.
#' @param dummy_reorder When `TRUE`, dummy-coded hierarchies are reordered to potentially improve the structure of output variables.
#' @param combine_vars When `TRUE`, an algorithm is applied to potentially reduce the number of output variables needed.
#' @param ... Additional parameters passed to \code{\link{AutoHierarchies}}
#'
#' @return Named list of data frames
#' @seealso \code{\link{vars_to_hierarchies}}
#' @export
#'
#' @examples
#' # Examples based on those from AutoHierarchies
#' # You may also try converting other examples from AutoHierarchies
#'
#' z <- SSBtoolsData("sprt_emp_withEU")
#' year_formula <- c("y_14 = 2014", "y_15_16 = y_all - y_14", "y_all = 2014 + 2015 + 2016")
#' geo_dim_list <- FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
#' age_hierarchy <- SSBtoolsData("sprt_emp_ageHier")
#'
#' hierarchies_as_vars(list(age = age_hierarchy, geo = geo_dim_list, year = year_formula))
#' hierarchies_as_vars(list(age = age_hierarchy, geo = geo_dim_list, year = year_formula), 
#'                     singleVars = TRUE)
#'                     
#' # NAs are included in data when necessary
#' hierarchies_as_vars(list(f = c("AB = A + B", "AC = A + C", "CD = C + D", "ABCD = AB + CD")))                     
#'                     
hierarchies_as_vars <- function(hierarchies,
                                name_function = function(name, level) paste0(name, "_level_", level),
                                single_vars = FALSE, 
                                from_dummy = NA, 
                                dummy_reorder = TRUE,
                                combine_vars = TRUE,
                                ...) {
  
  if (single_vars) {
    from_dummy <- TRUE
  }
  
  if (is.nan(from_dummy)) {
    message_here <- message 
  } else {
    message_here <- function(x) NULL
  }
  
  auto_hierarchies <- AutoHierarchies(hierarchies = hierarchies, ...)
  dummy_hierarchies <- DummyHierarchies(auto_hierarchies)
  
  if (dummy_reorder) {
    dummy_hierarchies <- fun_dummy_reorder(dummy_hierarchies, 
                                      auto_hierarchies, 
                                      message = message_here)
  }
  
  vars <- as.list(rep(FALSE, length(auto_hierarchies)))
  names(vars) <- names(auto_hierarchies)
  
  for (i in seq_along(auto_hierarchies)) {
    if (isFALSE(from_dummy) | is.na(from_dummy)) {
      vars[[i]] <- nice_hierarchy_to_vars(dummy_hierarchies[[i]], 
                                          auto_hierarchies[[i]],
                                          message = message_here)
      if (isFALSE(from_dummy) & isFALSE(vars[[i]])) {
        stop("FALSE from_dummy not working")
      }
    }
    if (isFALSE(vars[[i]])) {
      vars[[i]] <- dummy_to_vars(dummy_hierarchies[[i]], single_vars = single_vars)
    }
    names(vars[[i]])[1] <- names(vars)[i]
    if (combine_vars & !single_vars){
      vars <- lapply(vars, fun_combine_vars)     
    }
    if (!single_vars) {
      for (j in seq_len(ncol(vars[[i]]) - 1)) {
        names(vars[[i]])[j + 1] <- name_function(names(vars)[i], j)
      }
    }
  }
  
  vars
}

#' Transform hierarchies coded as Variables to "to-from" format 
#' 
#' A kind of reverse operation of \code{\link{hierarchies_as_vars}}
#'
#' @param var_hierarchies As output from \code{\link{hierarchies_as_vars}}
#'
#' @return List of hierarchies
#' 
#' @export
#' 
#' @examples
#' 
#' a <- hierarchies_as_vars(list(f = 
#'        c("AB = A + B", "CD = C + D", "AC = A + C", "ABCD = AB + CD")))
#' a
#' 
#' vars_to_hierarchies(a)
vars_to_hierarchies <- function(var_hierarchies) {
  if (any(!sapply(var_hierarchies, is.data.frame))) {
    stop("Input must be a list of data frames")
  }
  if (any(sapply(var_hierarchies, function(x) anyNA(x[[1]])))) {
    stop("The first column cannot have missing values")
  }
  lapply(var_hierarchies, vars_to_hierarchies_1)
}

vars_to_hierarchies_1 <- function(a) {
  z <- data.frame(mapsFrom = character(0), mapsTo = character(0), 
                  sign = integer(0), level = integer(0))
  
  for (i in SeqInc(2, ncol(a))) {
    x <- a[c(1, i)]
    x <- x[!is.na(x[[2]]), , drop = FALSE]
    names(x) <- c("mapsFrom", "mapsTo")
    z <- rbind(z, cbind(x, sign = 1L, level = i - 1L))
  }
  rownames(z) <- NULL
  z
}



nice_hierarchy_to_vars <- function(dummy_hierarchy, auto_hierarchy, message) {
  
  if (any(duplicated(auto_hierarchy$mapsFrom))) {
    message("duplicated(auto_hierarchy$mapsFrom)")
    return(FALSE)
  }
  
  unique_auto_2 <- unique(auto_hierarchy[c("mapsTo", "level")])
  if (any(duplicated(unique_auto_2$mapsTo))) {
    message("duplicated(unique_auto_2$mapsTo)")
    return(FALSE)
  }
  
  flat <- HierarchyFromDummy(dummy_hierarchy)
  if (any(flat$sign != 1)) {
    message("any(flat$sign != 1)")
    return(FALSE)
  }
  
  ma <- match(flat$mapsTo, unique_auto_2$mapsTo)
  flat$level_original <- unique_auto_2$level[ma]
  for (j in 1:max(flat$level_original)) {
    if (any(duplicated(flat$mapsFrom[flat$level_original == j]))) {
      message("duplicated(flat$mapsFrom[flat$level_original == j])")
      return(FALSE)
    }
  }
  
  x <- unique(flat["mapsFrom"])
  for (i in seq_len(max(flat$level_original))) {
    ma <- match(flat[flat$level_original == i, "mapsFrom"], x$mapsFrom)
    y <- data.frame(mapsTo = rep(NA, nrow(x)))
    y[ma, "mapsTo"] <- flat[flat$level_original == i, "mapsTo"]
    x <- cbind(x, y)
  }
  rownames(x) <- NULL
  x
}

dummy_to_vars <- function(dummy, single_vars = FALSE, first_name = "INPUT") {
  if (!all(unique(As_TsparseMatrix(dummy)@x) %in% c(0, 1))) {
    stop("Only 0 and 1 allowed in dummy matrix")
  }
  
  x <- t(dummy)
  n <- nrow(x)
  z <- vector("list", ncol(x) + 1)
  z[[1]] <- rownames(x)
  use_z <- rep(FALSE, length(z))
  use_z[1] <- TRUE
  colnames_x <- colnames(x)
  names_z <- c(first_name, colnames(x))
  names(z) <- names_z
  
  check <- FALSE
  for (i in seq_len(ncol(x))) {
    xi1 <- x[, i] == 1
    if (check) {
      if (any(!is.na(z[[names_z[j]]][xi1]))) {
        check <- FALSE
      }
    }
    if (!check) {
      j <- i + 1
      use_z[j] <- TRUE
      z[[names_z[j]]] <- rep(NA, n)
      check <- !single_vars
    }
    z[[names_z[j]]][xi1] <- colnames_x[i]
  }
  if (single_vars) {
    return(as.data.frame(z))
  }
  as.data.frame(z[use_z])
}



fun_dummy_reorder <- function(dummyHierarchies, autoHierarchies, message) {
  for (i in seq_along(dummyHierarchies)) {
    dummyHierarchies[[i]] <- fun_dummy_reorder1(dummyHierarchies[[i]], 
                                           autoHierarchies[[i]],  
                                           message = message)
  }
  dummyHierarchies
}


fun_dummy_reorder1 <- function(dummyHierarchy, autoHierarchy, message) {
  if (!any(diff(autoHierarchy$level) < 0) | any(autoHierarchy$sign < 0)) {
    ord <- match(unique(autoHierarchy$mapsTo), rownames(dummyHierarchy))
    sum1 <- sum(rowSums(dummyHierarchy) * seq_len(nrow(dummyHierarchy)))
    sum2 <- sum(rowSums(dummyHierarchy)[ord] * seq_len(nrow(dummyHierarchy)))
    if (sum2 >= sum1) {
      dummyHierarchy <- dummyHierarchy[ord, , drop = FALSE]
      message("reorder")
    } else {
      message(":")
    }
  }
  dummyHierarchy
}

fun_combine_vars <- function(x) {
  recursive <- FALSE
  if (anyNA(x)) {
    m <- As_TsparseMatrix(crossprod(as.matrix(!is.na(x))) == 0)
    ii <- integer(0)
    jj <- integer(0)
    ord <- order(m@i)
    i <- m@i[ord] + 1L
    j <- m@j[ord] + 1L
    while (length(i)) {
      recursive <- TRUE
      ii <- c(ii, i[1])
      jj <- c(jj, j[1])
      ind <- i == i[1] | i == j[1] | j == i[1] | j == j[1]
      i <- i[!ind]
      j <- j[!ind]
    }
    if (recursive) {
      for (k in seq_along(ii)) {
        isjk <- !is.na(x[jj[k]])
        if (any(!is.na(x[[ii[k]]][isjk]))) {
          stop("fun_combine_vars algorithm is wrong")
        }
        x[[ii[k]]][isjk] <- x[[jj[k]]][isjk]
      }
      x <- fun_combine_vars(x[-jj])
    }
  }
  x
}
 
