#' Difference and Sum Groups
#'
#' This function is a wrapper around [RowGroups()] for the specific case where the input 
#' contains two columns. It calls `RowGroups()` with `returnGroups = TRUE`, and extends 
#' the resulting data frame of unique code combinations with additional information 
#' about common groups, difference groups, and sum groups.
#'
#' @details
#' The returned list contains the same elements as from [RowGroups()], but with an 
#' extended `groups` data frame. The columns describe relationships between the two 
#' input columns as follows:
#'
#' * **is_common** — `TRUE` when the two codes on the row are identical.  
#' * **is_child_1**, **is_child_2** — `TRUE` when the code in the column is a subset or 
#' subgroup of a code in the other column.  
#' * **common** — identical code pairs, formatted using `sep_common`.  
#' * **diff_1_2**, **diff_2_1** — difference groups. The first element is the parent 
#'   from the source column, followed by one or more child codes from the opposite 
#'   column, joined using `sep_diff`.  
#' * **sum_1_2**, **sum_2_1** — sum groups where a parent code in one column equals the 
#'   sum of several codes in the other.
#'
#' @param x A data frame with exactly two columns.
#' @param ... Additional arguments passed to `RowGroups()`.
#' 
#' @param hiddenNA Logical. When `TRUE` (default), missing codes (`NA`) are treated as 
#' hidden categories — they are not available for computing difference and sum groups. 
#' See *Note* for details on how this differs from the `NAomit` parameter in `RowGroups()`.
#' 
#' @param sep_common A character string used in the `common` column to separate codes 
#' that are identical across the two input columns.
#' @param sep_diff A character string used in the `diff_1_2` and `diff_2_1` columns to 
#' indicate difference groups. The first column contains the parent code, and one or more 
#' child codes from the other column are subtracted.
#' @param sep_sum A character vector of one or two elements used in the `sum_1_2` and 
#' `sum_2_1` columns to describe relationships where a code in one column represents the 
#' sum of several codes in the other. The first element (`sep_sum[1]`) acts as an equality 
#' sign, and the second element (`sep_sum[2]`) acts as a plus sign. If `sep_sum` has 
#' length 1, the same value is used for both positions.
#' 
#' 
#' @param outputNA Character string used to represent `NA` values within the newly 
#' constructed text strings in the additional output columns. 
#' Only relevant when `hiddenNA = FALSE`.
#'
#' @returns A list (as returned by `RowGroups()`), where the `groups` data frame is 
#' extended with additional descriptive columns indicating common, difference, and sum 
#' relationships between the two code columns.
#' 
#' @note
#' The parameter `NAomit` from `RowGroups()` can still be set via `...`, but using it 
#' will remove rows containing `NA` before processing. The relationships found will then 
#' reflect the reduced data, which is usually not the intended behaviour when identifying 
#' relationships between code sets.
#' 
#' @seealso [data_diff_groups()] for adding the results back as new columns in the data frame.
#'
#' @export
#'
#' @examples
#' 
#' df <- SSBtoolsData("code_pairs")
#' 
#' df
#' 
#' diff_groups(df)
#' 
#' 
#' d2 <- SSBtoolsData("d2")
#' diff_groups(d2[1:2])$groups
#' diff_groups(d2[2:3])$groups
#'                            
#' 
diff_groups <- function(x, ...,
                        hiddenNA = TRUE,
                        sep_common = "_=_", sep_diff = "_-_", sep_sum = c("_=_", "_+_"),
                        outputNA = "NA") {
  
  sep_sum <- rep_len(sep_sum, 2)
  
  if (ncol(x) != 2) {
    stop("ncol(x) must be 2")
  }
  
  rg <- RowGroups(x, returnGroups = TRUE, ...)
  
  g <- rg$groups
  
  id <- seq_len(nrow(g))
  
  u1 <- unique_once(g[[1]])
  u2 <- unique_once(g[[2]])
  
  common_id <- intersect(u1, u2)
  t1 <- setdiff(u1, common_id)
  t2 <- setdiff(u2, common_id)
  
  g$is_common <- FALSE
  g$is_child_1 <- FALSE
  g$is_child_2 <- FALSE
  
  g$is_common[common_id] <- TRUE
  g$is_child_1[t1] <- TRUE
  g$is_child_2[t2] <- TRUE
  
  if (hiddenNA) {
    g$is_common[is.na(g[[1]]) | is.na(g[[2]])] <- FALSE
    g$is_child_1[is.na(g[[1]])] <- FALSE
    g$is_child_2[is.na(g[[2]])] <- FALSE
    g$is_child_1[is.na(g[[2]])] <- FALSE
    g$is_child_2[is.na(g[[1]])] <- FALSE
    
    outputNA <- as.character(outputNA)
    
  } 
  
  g$common <- paste(g[[1]], g[[2]], sep = sep_common)
  g$common[!g$is_common] <- NA
  
  dc12 <- diff_cells(g[2:1], g[g$is_child_2, 2:1], sep_diff, sep_sum, hiddenNA, outputNA)
  dc21 <- diff_cells(g[1:2], g[g$is_child_1, 1:2], sep_diff, sep_sum, hiddenNA, outputNA)
  
  g$diff_1_2 <- dc12$d
  g$diff_2_1 <- dc21$d
  
  g$sum_1_2 <- dc12$s
  g$sum_2_1 <- dc21$s
  
  
  rg$groups <- g
  rg
}


unique_once <- function(x) {
  which(!duplicated(x) & !duplicated(x, fromLast = TRUE))
}



diff_cells <- function(orig, dcols, sep_diff, sep_sum, hiddenNA, outputNA) {
  
  if (!hiddenNA) {
    orig[is.na(orig)] <- outputNA
    dcols[is.na(dcols)] <- outputNA
  }
  
  d <- orig[[2]]
  d[!(orig[[2]] %in% dcols[[2]])] <- NA
  
  ua <- unique(d[!is.na(d)])
  
  d[(orig[[1]] %in% dcols[[1]])] <- NA
  
  ub <- unique(d[!is.na(d)])
  
  sum_codes <- setdiff(ua, ub)
  
  s <- rep(NA_character_, length(d))
  
  for (k in sum_codes) {
    sc <- paste(k, paste(dcols[[1]][dcols[[2]] == k], collapse = sep_sum[2]), sep = sep_sum[1])
    s[orig[[2]] == k] <- sc
  }
  
  dcols <- dcols[!(dcols[[2]] %in% sum_codes), , drop = FALSE]
  
  d_ <- d
  while (nrow(dcols)) {
    ma <- match(d_, dcols[[2]])
    d[!is.na(ma)] <- paste(d[!is.na(ma)], dcols[[1]][ma[!is.na(ma)]], sep = sep_diff)
    dcols <- dcols[-ma[!is.na(ma)], ]
  }
  list(d = d, s = s)
}



#' Add diff_groups results as columns in a data frame
#'
#' `data_diff_groups()` is a wrapper around [diff_groups()] that runs the same analysis
#' on two variables in a data frame and adds selected results back as new columns.
#'
#' @param data A data frame containing the variables to be compared.
#' @param input_vars Character vector of length two specifying the names of the two 
#' variables in `data` to be compared.
#' @param output_vars Named character vector defining which variables from the group 
#' results are added to `data`, and what their names will be in the output.
#' @param ... Additional arguments passed to [diff_groups()].
#'
#' @returns A data frame identical to `data`, but with additional variables describing
#' relationships between the two specified columns.
#' 
#' @export
#'
#' @examples
#' df <- cbind(v1 = 1, SSBtoolsData("code_pairs"), v4 = 4)
#' 
#' data_diff_groups(df, input_vars = c("code_1", "code_2"))
#'
data_diff_groups <- function(data, 
                             input_vars, 
                             output_vars = c(is_common = "is_common", 
                                             diff_1_2 = "diff_1_2", 
                                             diff_2_1 = "diff_2_1", 
                                             sum_1_2 = "sum_1_2",
                                             sum_2_1 = "sum_2_1"), ...) {
  
  dg <- diff_groups(data[input_vars], ...)
  
  dg$groups <- dg$groups[names(output_vars)]
  names(dg$groups) <- output_vars
  
  to_cbind <- dg$groups[dg$idx, , drop = FALSE]
  rownames(to_cbind) <- NULL
  
  cbind(data, to_cbind)
}







