
# Had to be included in helper file to work


ff <- function(a1 = 0, a2 = 0, a3 = 0, ...) {
  f5(a1, a2, a3, ...)
}

f5 <- function(a1 = 0, a2 = 0, a3 = 0, a4 = 0, a5 = 0) {
  sum(a1 + 2 * a2 + 3 * a3 + 4 * a4 + 5 * a5)
}

zz <- data.frame(nr = c(1:2, 1:2), c1 = 1, c2 = 2, c3 = 3, c4 = 4)


amf <- function(...) {
  as.vector(as.matrix(aggregate_multiple_fun(
    data = zz, 
    by = zz["nr"], 
    vars = list(ff = "c1", f5 = c("c1", "c2", "c3"), f5 = c("c1", "c2", "c4")),
    fun = c(ff = function(a1 = 0, a2 = 0, a3 = 0, ...) {f5(a1, a2, a3, ...)}, f5 = f5), # Error within check if ff is not explicit
    ...)[2, 2:4]))
}



########

za <- SSBtoolsData("sprt_emp_withEU")
za$age[za$age == "Y15-29"] <- "young"
za$age[za$age == "Y30-64"] <- "old"
names(za)[names(za) == "ths_per"] <- "ths"
za$y <- 1:18 + round(1/9, 5)

my_range2 <- function(x, do_round = FALSE, mdigits = 0, ...) {
  minx <- min(x)
  maxx <- max(x)
  if (do_round) {
    minx <- round(minx, digits = mdigits)
    maxx <- round(maxx, ...)
  }
  c(min = minx, max = maxx)
}


ma <- function(..., dim_var = NULL, formula = ~age:year + geo, frame_return = FALSE,
               sum_vars = c("y", "ths"),
               fun_vars = c(sum = "ths", mean = "y", ra = "y")) {
  a <- model_aggregate(za, dim_var = dim_var, formula = formula, 
                       sum_vars = sum_vars, 
                       fun_vars = fun_vars, fun = c(sum = sum, mean = mean, ra = my_range2), 
                       verbose = FALSE, ...)
  if (frame_return) {
    return(a)
  }
  
  out_names <- c("y", "ths", "ths_sum", "y_mean", "y_ra.min", "y_ra.max")
  out_names <- out_names[out_names %in% names(a)]
  b <- a[a$age == "old" & a$year == "2016", out_names]
  
  as.vector(as.matrix(b))
}


####################
# For test based on application + more complicated 
# Function id_bidrag in application rewritten to produce mathematical expressions
# Function id_bidrag_matrix more complicated since fun_vector parameter


id_bidrag <- function(ind, sign, data) {
  if (!sum(ind)) {
    return(0)
  }
  paste0("(", sign[ind > 0], ")*(", data[["value"]][ind], ")", collapse = " + ")
}

id_bidrag_vector <- function(sign, ind, ind_matrix, data, fun_id_bidrag = id_bidrag) {
  indm <- ind_matrix[ind, , drop = FALSE]
  apply(indm, 2, fun_id_bidrag, sign, data)
}


id_bidrag_matrix <- function(sign_matrix, ind_matrix, data, fun_id_bidrag = id_bidrag) {
  ind_matrix <- as.matrix(ind_matrix)
  df_seq_len <- data.frame(ind = seq_len(nrow(ind_matrix)))
  a <- as.matrix(dummy_aggregate(data = df_seq_len, x = sign_matrix, 
                                 vars = c(ibv = "ind"), 
                                 fun = c(ibv = function(..., fun_vector, fun_vector_data, fun_id_bidrag)
                                   fun_vector(..., data = fun_vector_data, 
                                              fun_id_bidrag = fun_id_bidrag)),
                                 fun_vector = id_bidrag_vector,
                                 ind_matrix = as.matrix(ind_matrix), fun_vector_data = data, 
                                 fun_id_bidrag = fun_id_bidrag, dummy = FALSE, dots2dots = TRUE, 
                                 forward_dots = TRUE, do_unmatrix = FALSE, keep_names = FALSE)[[1]])
  colnames(a) <- colnames(ind_matrix)
  rownames(a) <- colnames(sign_matrix)
  a
}

df_idm <- data.frame(value = 11 * 1:9)
ind_matrix <- structure(c(0, 8, 5, 7, 0, 0, 0, 0, 0, 0, 0, 8, 5, 7, 1, 2, 3, 
                          4, 5, 6, 7, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 9),
                        dim = c(7L,5L), 
                        dimnames = list(NULL, c("im1", "im2", "im3", "im4", "im5")))

sign_matrix <- structure(c(0, 1, -1, 2, 0, 0, 0, 0, 0, 0, 0, 1, -1, 2, 1, 0, 
                           0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 1, 
                           1, 1, 1, 1, 1, 1), dim = 7:6, 
                         dimnames = list(NULL, c("sm1", "sm2", "sm3", "sm4", "sm5", "sm6")))




