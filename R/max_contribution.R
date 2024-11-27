#' Find major contributions to aggregates
#'
#' This function identifies the largest contributions to aggregates. Aggregates are 
#' assumed to be calculated using a dummy matrix with the formula:
#' \code{z = t(x) \%*\% y}. 
#' For each aggregate, the `n` largest contributions are identified.
#'
#' @param x A (sparse) dummy matrix 
#' @param y A numeric vector of input values (contributions).
#' @param n Integer. The number of largest contributors to identify for each aggregate.
#'          Default is 1.
#' @param id An optional vector for grouping. When non-NULL, major contributions are 
#'           found after aggregation within each group specified by `id`. 
#'           Aggregates with missing `id` values are excluded.
#' @param output A character vector specifying the desired output. Possible values:
#'   - `"y"`: A matrix with the largest contributions in the first column, the second largest in the second column, and so on.
#'   - `"id"`: A matrix of IDs associated with the largest contributions. If an `id` vector is provided, it returns these IDs; otherwise, it returns indices.
#'   - `"n_contr"`: An integer vector indicating the number of contributors to each aggregate.
#'   - `"n_0_contr"`: An integer vector indicating the number of contributors that contribute a value of 0 to each aggregate.
#'   - `"n_non0_contr"`: An integer vector indicating the number of contributors that contribute a nonzero value to each aggregate.
#'   - `"sums"`: A numeric vector containing the aggregate sums of `y`.
#'   - `"n_contr_all"`, `"n_0_contr_all"`, `"n_non0_contr_all"`, `"sums_all"`: 
#'         Same as the corresponding outputs above, but without applying the `remove_fraction` parameter.
#' @param drop Logical. If TRUE (default) and `output` has length 1, 
#'   the function returns the single list element directly instead of a list containing one element. 
#' @param decreasing Logical. If TRUE (default), finds the largest contributors. 
#'                   If FALSE, finds the smallest contributors.
#' @param remove_fraction A numeric vector containing values in the interval `[0, 1]`, specifying contributors to be removed when identifying the largest contributions. 
#'   - If an `id` vector is provided, `remove_fraction` must be named according to the IDs of the contributors to be removed.
#'   - If no `id` vector is provided, the length of `remove_fraction` must match the length of `y`. In this case, contributors not to be removed should have a value of `NA` in `remove_fraction`.
#'   - The actual values in `remove_fraction` are used for calculating `"sums"` (see description above).
#' @param do_abs Logical. If TRUE (default), uses the absolute values of the summed contributions. 
#'   The summation is performed for all contributions from the same contributor, 
#'   within each aggregate being computed.
#'
#' @return A list or a single element, depending on the values of the `output` and `drop` parameters.
#' 
#' @export
#' @importFrom Matrix Diagonal
#' @importFrom methods new
#' 
#' @examples
#' 
#' z <- SSBtoolsData("magnitude1")
#' a <- ModelMatrix(z, formula = ~sector4 + geo, crossTable = TRUE)
#' 
#' cbind(a$crossTable, 
#'       y = max_contribution(x = a$modelMatrix, y = z$value, n = 2), 
#'       id = max_contribution(x = a$modelMatrix, y = z$value, n = 2, output = "id"))
#' 
#' cbind(a$crossTable, 
#'       y = max_contribution(x = a$modelMatrix, y = z$value, n = 3, id = z$company), 
#'       id = max_contribution(a$modelMatrix, z$value, 3, id = z$company, output = "id"))
#' 
#' max_contribution(x = a$modelMatrix, 
#'                  y = z$value, 
#'                  n = 3, 
#'                  id = z$company, 
#'                  output = c("y", "id", "n_contr", "sums"))
#' 
#' as.data.frame(
#'   max_contribution(x = a$modelMatrix, 
#'                    y = z$value, 
#'                    n = 3, 
#'                    id = z$company, 
#'                    output = c("y", "id", "n_contr", "sums", "n_contr_all", "sums_all"), 
#'                    remove_fraction = c(B = 1)))
#' 
max_contribution <- function(x, 
                             y, 
                             n = 1, 
                             id = NULL, 
                             output = "y", 
                             drop = TRUE,
                             decreasing = TRUE,
                             remove_fraction = NULL,
                             do_abs = TRUE) {
  
  
  out_names <- c("y", "id", 
                 "n_contr",     "n_0_contr",     "n_non0_contr",     "sums", 
                 "n_contr_all", "n_0_contr_all", "n_non0_contr_all", "sums_all")
  out <- vector("list", length( out_names))
  names(out) <-  out_names
  
  output <- names(out) %in% output
  names(output) <- names(out)
  
  try_return <- function() {
    ok_out <- !sapply(out, is.null)
    if (all(ok_out[output])) {
      if (drop & sum(output) == 1) {
        return(out[[which(output)]])
      }
      return(out[which(output)])
    }
    NULL
  }
  
  
  if (!is.null(remove_fraction)) {
    if (length(remove_fraction[!is.na(remove_fraction)])) {
      if (min(remove_fraction, na.rm = TRUE) < 0 | max(remove_fraction, na.rm = TRUE) >
          1) {
        stop("remove_fraction must be within the interval [0, 1]")
      }
    }
  }
  
  if (is.null(id)) {
    id <- seq_len(nrow(x))
    fid <- id
    id_input <- FALSE
    if (!is.null(remove_fraction)) {
      if (length(remove_fraction) != nrow(x)) {
        stop("wrong length of remove_fraction")
      }
    }
  } else {
    id_input <- TRUE
    if (!is.null(remove_fraction)) {
      if (!length(names(remove_fraction))) {
        stop("remove_fraction must be named")
      }
      if (any(duplicated(names(remove_fraction)))) {
        stop("duplicated remove_fraction names")
      }
      if (!all(names(remove_fraction) %in% unique(id))) {
        warning("remove_fraction names not in id")
      }
    }
    if (length(id) != nrow(x)) {
      stop("Incorrect length of id")
    }
    
    if (anyNA(id)) {
      rows <- !is.na(id)
      id <- id[rows]
      x <- x[rows, , drop = FALSE]
      y <- y[rows]
    }
    
    fid <- factor(id)
    id <- as.integer(fid)
    fid <- levels(fid)
    
    if (!is.null(remove_fraction)) {
      remove_fraction_input <- remove_fraction
      remove_fraction <- rep(NA_real_, max(id))
      ma <- match(fid, names(remove_fraction_input))
      remove_fraction[!is.na(ma)] <- remove_fraction_input[ma[!is.na(ma)]]
    }
    
  }
  
  if (!is.null(remove_fraction)) {
    keep <- is.na(remove_fraction)
    remove_fraction[keep] <- 0
  }
  
  
  xT <- As_TsparseMatrix(x) 
  
  if (id_input) {
    gT <- new("dgTMatrix", i = 0:(nrow(x) - 1L), j = id - 1L, x = y, Dim = c(nrow(xT), max(id)))
    gT <- As_TsparseMatrix(crossprod(gT, xT),  do_drop0 = FALSE)
    if(do_abs) {
      gT <- abs(gT)
    }
    xM <- data.frame(y = gT@x, col = gT@j + 1, gr = gT@i + 1)
  } else {  # same but simpler calculation
    if(do_abs) {
      y <- abs(y)
    }
    xM <- data.frame(y = y[xT@i + 1], col = xT@j + 1, gr = id[xT@i + 1])
  }
  
  if (output[["sums_all"]] | (output[["sums"]] & is.null(remove_fraction))) {
    if (id_input) {
      out$sums_all <- colSums(gT)
    } else {
      out$sums_all <- as.matrix(crossprod(xT, y))[, 1]
    }
    if (output[["sums"]] & is.null(remove_fraction)) {
      out$sums <- out$sums_all
    }
  }
  
  if (!is.null(remove_fraction) & output[["sums"]]) {
    if (id_input) {
      gT <- Diagonal(x = 1-remove_fraction)  %*% gT
      out$sums <- colSums(gT) 
    }
    else {
      out$sums <- as.matrix(crossprod(xT, y * (1-remove_fraction)))[, 1]
    }
  }
  
  if (!is.null(tr <- try_return())) return(tr)
  
  
  if (output[["n_contr_all"]]) {
    out$n_contr_all <- as.vector(table_all_integers(xM[, "col"], ncol(x)))
  }
  if (output[["n_0_contr_all"]]) {
    out$n_0_contr_all <- as.vector(table_all_integers(xM[, "col"][xM[, "y"]==0], ncol(x)))
  }
  if (output[["n_non0_contr_all"]]) {
    out$n_non0_contr_all <- as.vector(table_all_integers(xM[, "col"][xM[, "y"]!=0], ncol(x)))
  }
  
  if (!is.null(tr <- try_return())) return(tr)
  
  
  if (!is.null(remove_fraction)) {
    xM <- xM[keep[xM[, "gr"]], , drop = FALSE] 
  }
  
  xM <- as.matrix(xM)  # Needed since empty index below
  
  xM[, "y"] <- -decreasing * xM[, "y"]
  xM <- SortRows(xM) 
  xM[, "y"] <- -decreasing * xM[, "y"]
  
  seqCol <- seq_len(ncol(x))
  
  if (output[["n_contr"]]) {
    out$n_contr <- as.vector(table_all_integers(xM[, "col"], ncol(x)))
  }
  if (output[["n_0_contr"]]) {
    out$n_0_contr <- as.vector(table_all_integers(xM[, "col"][xM[, "y"]==0], ncol(x)))
  }
  if (output[["n_non0_contr"]]) {
    out$n_non0_contr <- as.vector(table_all_integers(xM[, "col"][xM[, "y"]!=0], ncol(x)))
  }
  
  if (!is.null(tr <- try_return())) return(tr)
  
  
  out$y <- matrix(NA_integer_, ncol(x), n)
  
  if (output[["id"]]) {
    out$id <- matrix(ifelse(id_input, NA_character_, NA_integer_), ncol(x), n)
  }
  
  for (i in seq_len(n)) {
    if (i > 1) {
      xM[ma, "col"] <- 0
    }
    ma <- match(seqCol, xM[, "col"])
    out$y[, i] <- xM[ma, "y"]
    if (output[["id"]]) {
      out$id[, i] <- fid[xM[ma, "gr"]]
    }
  }
  
  if (!is.null(tr <- try_return())) return(tr)
  
  warning("Something went wrong while generating output.")
  
  out
}


