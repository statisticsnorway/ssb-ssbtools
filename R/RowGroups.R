#' Create numbering according to unique rows
#'
#' @param x Data frame or matrix
#' @param returnGroups   When TRUE unique rows are returned
#' @param returnGroupsId When TRUE Index of unique rows are returned
#' @param NAomit When `TRUE`, rows containing NAs are omitted, and the corresponding index numbers are set to `NA`. 
#' @param pkg A character string indicating which package to use. 
#'            Must be either `"base"` for base R or `"data.table"` for `data.table`. Default is `"base"`.
#'
#' @return A vector with the numbering or, according to the arguments, 
#'         a list with more output.
#' @export
#' @author Øyvind Langsrud
#' 
#' @examples 
#' a <- data.frame(x = c("a", "b"), y = c("A", "B", "A"), z = rep(1:4, 3))
#' RowGroups(a)
#' RowGroups(a, TRUE)
#' RowGroups(a[, 1:2], TRUE, TRUE)
#' RowGroups(a[, 1, drop = FALSE], TRUE)
RowGroups <- function(x, returnGroups = FALSE, returnGroupsId = FALSE, NAomit = FALSE, pkg = "base") {
  
  if (!(pkg %in% c("base", "data.table"))) {
    stop('pkg must be "base" or "data.table"')
  } 
    
  if (NROW(x) == 0) 
    return(RowGroups0rows(x = x, returnGroups = returnGroups, returnGroupsId = returnGroupsId))
  
  if (pkg == "data.table") {
    return(RowGroupsDT(data = x, returnGroups = returnGroups, returnGroupsId = returnGroupsId, NAomit = NAomit))
  }
  
  if (NAomit) {
    return(RowGroupsNAomit(x = x, returnGroups = returnGroups, returnGroupsId = returnGroupsId, NAomit = FALSE))
  }
  
  xInteger <- AsFactorInteger(x)
  if (!is.null(xInteger)) {
    if (NCOL(xInteger) == 1) {
      ix <- order(xInteger)
      dp <- duplicated(xInteger)
    } else {
      ix <- SortRows(xInteger, index.return = TRUE)
      dp <- duplicated(xInteger)
    }
  } else {
    ix <- SortRows(x, index.return = TRUE)
    dp <- duplicated(x)
  }
  a <- rep(NA_integer_, length(dp))
  a[ix] <- cumsum(!dp[ix])
  if (!(returnGroups | returnGroupsId)) 
    return(a)
  
  out <- NULL
  out$idx <- a
  
  idg <- ix[!dp[ix]]
  
  if (returnGroups) {
    out$groups <- x[idg, , drop = FALSE]
    row.names(out$groups) <- NULL
  }
  
  if (returnGroupsId) 
    out$idg <- idg
  
  out
}



#' Fast alternative to `anyDuplicated()`
#'
#' Implemented similarly to [RowGroups()].  
#' 
#' With `data.table` input and the data.table package available, 
#' `anyDuplicated.data.table()` will be used.
#'
#' @param x A data frame, tibble, or data.table. 
#' @param cols Columns to check for duplicates.
#'
#' @returns Index of the first duplicate row, if any; otherwise 0.
#' @export
#'
#' @examples
#' z <- SSBtoolsData("power10to2")
#' head(z, 12)
#' tail(z)
#' 
#' any_duplicated_rows(z, c("A", "B"))
#' any_duplicated_rows(z, c("a", "A", "B"))
#' any_duplicated_rows(z, c("a", "A", "b"))
any_duplicated_rows <- function(x, cols = names(x)) {
  if (inherits(x, "data.table")) {
    if (requireNamespace("data.table", quietly = TRUE)) {
      return(anyDuplicated(x[, ..cols]))
    }
  } 
  as_data_frame <- !identical(class(x), "data.frame")
  n <- NROW(x)
  if (n == 0) {
    return(FALSE)
  }
  if (n > 30) {
    ad <- any_duplicated_rows_(x[1:10, cols, drop = FALSE], as_data_frame)
    if (ad) return(ad)
  }
  if (n > 3000) {
    ad <- any_duplicated_rows_(x[1:1000, cols, drop = FALSE], as_data_frame)
    if (ad) return(ad)
  }
  if (n > 3e+05) {
    ad <- any_duplicated_rows_(x[1:1e+05, cols, drop = FALSE], as_data_frame)
    if (ad) return(ad)
  }
  if (n > 3e+07) {
    ad <- any_duplicated_rows_(x[1:1e+07, cols, drop = FALSE], as_data_frame)
    if (ad) return(ad)
  }
  any_duplicated_rows_(x[, cols, drop = FALSE], as_data_frame)
}


any_duplicated_rows_ <- function(x, as_data_frame = FALSE) {
  if (as_data_frame) {
    x <- as.data.frame(x)
  }
  xInteger <- AsFactorInteger(x)
  if (!is.null(xInteger)) {
    return(anyDuplicated(xInteger))
  }
  anyDuplicated(x)
}



RowGroups0rows <- function(x, returnGroups = FALSE, returnGroupsId = FALSE) {
  if (!(returnGroups | returnGroupsId)) 
    return(integer(0))
  out <- NULL
  out$idx <- integer(0)
  if (returnGroups) 
    out$groups <- x
  if (returnGroupsId) 
    out$idg <- integer(0)
  out
}


AsFactorInteger <- function(x) {
  
  for (i in seq_len(NCOL(x))) x[, i] <- as.integer(factor(x[, i, drop = TRUE], exclude = NULL)) - 1
  
  
  hyperN <- rev(cumprod(as.numeric(apply(x, MARGIN = c(2), max) + 1)))[1]
  
  a <- hyperN + 1
  b <- hyperN - 1
  
  if (!is.finite(a - b)) 
    return(x)
  
  if (a - b != 2) 
    return(x)
  
  k <- rev(c(1, cumprod(rev(as.numeric(apply(x, MARGIN = c(2), max)) + 1))))[-1]
  
  for (i in seq_len(NCOL(x))) 
    x[, i] <- k[i] * x[, i, drop = TRUE]
  
  rowSums(x)
  
}



RowGroupsNAomit <- function(x, ...) {
  isNa <- rep(FALSE, NROW(x))
  for (i in seq_len(NCOL(x))) {
    isNa <- isNa | is.na(x[, i, drop = TRUE])
  }
  
  if (!any(isNa)) {
    return(RowGroups(x, ...))
  }
  
  rg1 <- rep(NA_integer_, NROW(x))
  
  rg <- RowGroups(x[!isNa, , drop = FALSE], ...)
  
  if (is.list(rg)) {
    rg1[!isNa] <- rg[[1]]
    rg[[1]] <- rg1
    if (!is.null(rg$idg)) {
      rg$idg <- which(!isNa)[rg$idg]
    }
  } else {
    rg1[!isNa] <- rg
    rg <- rg1
  }
  rg
}




RowGroupsDT <- function(data, returnGroups = FALSE, returnGroupsId = FALSE, NAomit = FALSE) {
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required but is not installed. Please install it first.")
  }
  
  # Convert to data.table
  dt <- data.table::as.data.table(data)
  
  # If NAomit is TRUE, handle rows with NA separately
  if (NAomit) {
    idx <- rep(NA, nrow(dt))  # Create an idx vector filled with NA
    complete_rows <- complete.cases(dt)  # Find rows without NA
    dt  <- dt[complete_rows]  # Keep only rows without NA for further processing
  } 
  
  # Create groups for all rows
  dt[, G_r0u_P := .GRP, by = names(dt)]
  
  # Update the idx vector based on whether NAomit is TRUE or FALSE
  if (NAomit) {
    idx[complete_rows] <- dt$G_r0u_P
  } else {
    idx <- dt$G_r0u_P
  }
  
  # groups: Get unique groups by taking the first row per group
  groups <- dt[, .SD[1], by = G_r0u_P]
  groups <- groups[, G_r0u_P := NULL]  # Remove the G_r0u_P column from groups
  
  
  # Sort groups by all columns
  sort_order <- do.call(order, groups[, names(groups), with = FALSE])
  
  # Sort groups based on sort_order
  groups <- groups[sort_order]
  
  # Update idx so that it points to the correct row in the sorted groups
  idx <- order(sort_order)[idx]
  if (!(returnGroups | returnGroupsId)) {
    return(idx)
  }
  
  out <- NULL
  out$idx <- idx
  
  if (returnGroups) {
    out$groups <- as.data.frame(groups)
  }
  
  if (returnGroupsId) {
    # idg: Row indices of the unique rows (one index per unique group)
    idg <- dt[, .I[1], by = G_r0u_P]$V1
    
    # Update idg to follow the same sorting order as groups
    idg <- idg[sort_order]
    
    # Final row indices in original data
    idg <- which(complete_rows)[idg]
    
    out$idg <- idg
  }
  
  out
}
# RowGroupsDT is written with help from ChatGPT


# To avoid problems when data.table not in Depends
.datatable.aware <- TRUE 

# To avoid check problems
utils::globalVariables(c(".GRP", ".I", ":=", "G_r0u_P", "..cols"))



