#' Find directly disclosive cells
#' 
#' Function for determining which cells in a frequency table can lead to 
#' direct disclosure of an identifiable individual, assuming an attacker has the
#' background knowledge to place themselves (or a coalition) in the table.
#' 
#' This function does not work on data containing hierarchical variables.
#' 
#' @param data the data set
#' @param freq vector containing frequencies
#' @param crossTable cross table of key variables produced by ModelMatrix
#' in parent function
#' @param primaryDims dimensions to be considered for direct disclosure.
#' @param unknowns vector of unknown values for each of the primary dimensions.
#' If a primary dimension does not contain unknown values, NA should be passed.
#' @param total string name for marginal values
#' @param unknown.threshold numeric for specifying a percentage for calculating
#' safety of cells. A cell is "safe" in a row if the number of unknowns exceeds
#' `unknown.threshold` percent of the row total.
#' @param coalition maximum number of units in a possible coalition, default 1
#' @param ... parameters from main suppression method
#'
#' @return list with two named elements, the first ($primary) being a logical vector
#' marking directly disclosive cells, the second ($numExtra) a data.frame containing
#' information regarding the dimensions in which the cells are directly disclosive.
#' 
#' @export
#'
#' @examples
#' extable <- data.frame(v1 = rep(c('a', 'b', 'c'), times = 4),
#'             v2 = c('i','i', 'i','h','h','h','i','i','i','h','h','h'),
#'             v3 = c('y', 'y', 'y', 'y', 'y', 'y','z','z', 'z', 'z', 'z', 'z'),
#'             freq = c(0,0,5,0,2,3,1,0,3,1,1,2))
#' ex_freq <- c(18,10,8,9,5,4,9,5,4,2,0,2,1,0,1,1,0,1,3,2,1,3,2,1,0,0,0,13,8,5,
#'              5,3,2,8,5,3)
#' cross <- ModelMatrix(extable,
#'                      dimVar = 1:3,
#'                      crossTable = TRUE)$crossTable
#' 
#' FindDisclosiveCells(extable, ex_freq, cross) 
FindDisclosiveCells <- function(data,
                       freq,
                       crossTable,
                       primaryDims = names(crossTable),
                       unknowns = rep(NA, length(primaryDims)),
                       total = rep("Total", length(primaryDims)),
                       unknown.threshold = 0,
                       coalition = 1,
                       suppressSmallCells = FALSE,
                       ...) {
  if (!is.numeric(unknown.threshold))
    stop(paste0("Given unknown.threshold: \"", unknown.threshold,
                "\". Must be a number between 0 and 100."))
  else if (as.numeric(unknown.threshold) < 0 | 
           as.numeric(unknown.threshold) > 100)
      stop(paste0("Given unknown.threshold: \"", unknown.threshold,
                  "\". Must be a number between 0 and 100."))
  
  if (length(primaryDims) > length(unknowns)) {
    warning("Length of input parameter unknowns less than input dimensions, 
            remaining filled with NA")
    unknowns <- c(unknowns, rep(NA, length(primaryDims)-length(unknowns)))
  }
  if (length(primaryDims) < length(unknowns)) {
    warning("Length of input dimensions is less than number of provided unknowns, 
            superfluous unknown values will be ignored.")
  }
  # number unique values in each dimension of primaryDims
  numval <- lapply(data[primaryDims],
                   function (x)
                     length(unique(x)))
  varnames <- colnames(crossTable)
  # dataframe for lagring av prikkestatus etter variabel. Kan effektifiseres 
  # (bl.a. fjerne Reduce senere), men for debugging gjøres det slikt nå
  out <- crossTable[primaryDims]
  # vector specifying whether each cell is a member of some unknown category
  is_unknown <- cross_unknowns(crossTable, primaryDims, unknowns)

  for (ind in 1:length(primaryDims)) {
    var <- primaryDims[ind]
    unknown <- unknowns[ind]
    is_total <- crossTable[[var]] == total[ind]
    between <- as.vector(varnames[varnames != var])
    rt <- freq[is_total]
    row_totals <- rt[Match(crossTable[between],
                                     crossTable[is_total, between, drop=FALSE])]
    
    # check whether cells are member of current dimension's unknown
    if (!is.na(unknown)) {
      vars_unknown <- crossTable[[var]] == unknown
      if (!any(vars_unknown))
        warning(paste0("Ingen tilfeller av \"", unknown, "\" funnet i ", var))
      
      a_unknown <- freq[vars_unknown]
      n_unknown <- a_unknown[Match(crossTable[between],
                                             crossTable[vars_unknown, between,
                                                        drop=FALSE])]
      # determine safe unknowns by p% rule if threshold is > 0,
      # otherwise presence of unknowns is considered safe
      safe_unknowns <- safe_unknown(threshold = unknown.threshold,
                                    rowtotals = row_totals,
                                    n_unknown = n_unknown)
    }
    else {
      vars_unknown <- FALSE
      safe_unknowns <- FALSE
    }
    # maximum freq per row, non-total and non-unknown
    row_max <- find_row_max(freq, crossTable, between, is_total, vars_unknown)
    if (!suppressSmallCells)
      disclosive <- (( freq > 0 & freq == row_max) & 
                     (coalition >= row_totals - row_max))
    else
      disclosive <- (( freq > 0 & freq <= coalition) & 
                       (row_max >= row_totals - coalition))
    prim <- !safe_unknowns & !is_unknown & !is_total &
           ((freq > 0 & freq == row_totals) |
           disclosive)
    out[var] <- prim
  }
  out <- as.data.frame(out)
  primary <- Reduce(`|`, out)
  names(out) <- paste0(names(out), "-prikk")
  list(primary = primary, numExtra = out)
}

# internal function for determining unknowns in FindDisclosiveCells
#
# @param crosstable cross table of key variables produced by ModelMatrix
# in parent function
# @param vars vector of variable names to be considered
# @param unknowns string vector of unknown values for each variable in `vars`
#
# @return logical vector marking relevant cells as containing unknowns
cross_unknowns <- function(crosstable, vars, unknowns) {
  ret <- FALSE
  for (ind in 1:length(vars)) {
    var <- vars[ind]
    unk <- unknowns[ind]
    if (!is.na(unk))
      ret <- ret | (crosstable[[var]] == unk)
  }
  ret
}

# internal function for unknown safety threshold in FindDisclosiveCells 
#
# @param threshold integer vector of length one between 0 and 100
# @param rowtotals vector containing row totals
# @param n_unknown vector containing number of unknowns in row
#
# @return logical vector marking relevant cells as safe according to unknown
# threshold rule
safe_unknown <- function(threshold, rowtotals, n_unknown) {
    min_unk <- threshold / 100 * (rowtotals-1)
    n_unknown > min_unk
}

# Internal function for finding maximum non-total, non-unknown row freq
#
# @param freq frequencies
# @param crossTable crossTable generated by ModelMatrix
# @param between variables specifying which row cell is in
# @param is_total boolean vector stating whether a cell is a total
# @param is_unknown boolean vector stating whether a cell is an unknown
#
# @return vector containing max non-total, non-unknown cell freq per row
find_row_max <- function(freq, crossTable, between, is_total, is_unknown) {
  ntu_freq <- rep(-Inf, length(freq))
  ntu_freq[!is_total & !is_unknown] <- freq[!is_total & !is_unknown]
  vals <- aggregate(list(maxes = ntu_freq),
                    crossTable[, between, drop = FALSE],
                    c, simplify = FALSE)
  fs <- vals$maxes[Match(crossTable[between], vals[between])]
  out <- sapply(fs, max)
  out
}
