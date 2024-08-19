

aggregate_by_pkg <- function(data, by, var, pkg = "base", include_na = FALSE, fun = sum, base_order = TRUE) {
  if (pkg == "base") {
    
    na_included <- rep(FALSE, length(by))
    
    if (include_na) {
      for (i in seq_along(by)) {
        if (anyNA(data[[by[i]]])) {
          na_included[i] <- TRUE
          if (is.integer(data[[by[i]]])) {
            data[[by[i]]][is.na(data[[by[i]]])] <- .Machine$integer.max - 3L
          } else if (is.numeric(data[[by[i]]])) {
            data[[by[i]]][is.na(data[[by[i]]])] <- 1.789e+308
          } else {
            if (is.factor(data[[by[i]]]) | is.logical(data[[by[i]]])) {
              data[[by[i]]] <- as.character(data[[by[i]]])
              warning(paste(names(data)[i], "changed from", class(data[[by[i]]]), "to character"))
            }
            data[[by[i]]][is.na(data[[by[i]]])] <- "~~~~~~M"
          }
        }
      }
    }
    
    result <- aggregate(data[var], data[by], fun)
    
    if (any(na_included)) {
      for (i in seq_along(by)) {
        if (na_included[i]) {
          if (is.integer(result[[by[i]]])) {
            result[[by[i]]][result[[by[i]]] == (.Machine$integer.max - 3L)] <- NA
          } else if (is.numeric(result[[by[i]]])) {
            result[[by[i]]][result[[by[i]]] == 1.789e+308] <- NA
          } else {
            result[[by[i]]][result[[by[i]]] == "~~~~~~M"] <- NA
          }
        }
      }
    }
    
    return(result)
  }
  
  if (pkg == "data.table") {
    dt <- as.data.table(data)
    if (!include_na) {
      dt <- dt[complete.cases(dt[, ..by]), ]
    }
    dt <- dt[, lapply(.SD, fun), by = by, .SDcols = var]
    
    if (base_order) {
      sort_order <- do.call(order, dt[, rev(by), with = FALSE])
      dt <- dt[sort_order]
    }
    
    return(as.data.frame(dt))
  }
  
  stop('pkg must be "base" or "data.table"')
}



