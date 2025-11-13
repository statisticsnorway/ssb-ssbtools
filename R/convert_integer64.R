#' Convert `integer64` data to base R numeric, integer, or character types
#'
#' Converts `integer64` data (from the **bit64** package) in a `data.frame`,
#' list, or vector to base R numeric, integer, or character types.
#'
#' Variables with class `integer64` often appear when reading data from Arrow
#' files, for example using [arrow::read_parquet()]. Arrow supports 64-bit
#' integer values, while the R language (and thus all R packages, including
#' the tidyverse) only supports 32-bit integers and 64-bit floating-point
#' numbers. These 64-bit integers therefore need conversion when loaded into R.
#'
#' When the input is a `data.frame` or list, conversion is performed **variable by variable**,
#' and only those with class `integer64` are modified.
#'
#' Depending on settings, `integer64` data are converted to base R `integer`,
#' `numeric` (double), or `character`.
#' 
#' Note that a simpler helper that always converts directly to `numeric`,
#' without any checks or dependency tests, can be defined as:                
#'                   
#'                   convert_integer64_to_numeric <- function(df) {
#'                     df[] <- lapply(df, function(x) {
#'                       if (inherits(x, "integer64")) as.numeric(x) else x
#'                     })
#'                     df
#'                   }
#'
#'
#' @param df A `data.frame`, list, or vector to process.
#' @param to_integer Character string controlling how conversion to integer is handled.
#'   The rule is applied **variable by variable**. Must be one of:
#'
#'   * `"never"` — always convert to `numeric`, never to `integer`
#'   * `"if_fits"` — convert to `integer` if all values fit within 32-bit range *(default)*
#'   * `"if_summable"` — convert to `integer` if the sum of absolute values fits within 32-bit range
#'   * `"always"` — always convert to `integer`, with potential coercion warnings  
#'   * `"always_quiet"` — always convert to `integer`, suppressing coercion warnings
#'
#' @param precision_loss Character string controlling what happens when
#'   64-bit integers cannot be represented exactly as 64-bit floating-point numbers.
#'   Must be one of:
#'
#'   * `"character"` — convert to `character` if the value cannot be represented exactly *(default)*
#'   * `"warn"` — convert to `numeric` and allow warnings about precision loss
#'   * `"quiet"` — convert to `numeric` but suppress such warnings
#'
#' @param always_character Logical. If `TRUE`, all `integer64` values are converted
#'   directly to `character`, overriding both `to_integer` and `precision_loss`.
#'   Default is `FALSE`.
#'
#' @return The same type of object as the input (`data.frame`, list, or vector),
#'   with all `integer64` values converted to base R `integer`, `numeric`, or
#'   `character` depending on settings.
#' @seealso [bit64::as.integer64()]
#' 
#' @note This function is written and documented with help from ChatGPT. 
#' 
#' @export
#'
#' @examples
#' if (requireNamespace("bit64", quietly = TRUE)) {
#'   x  <- bit64::seq.integer64(2025, 10^9, 3 * 10^8)
#'   print(x)
#'   
#'   print(convert_integer64(x*4, "always_quiet"))
#'   
#'   df <- data.frame(a = 11:14, b = x, c = 2 * x, d = 3 * x, e = x * x, f = c(22, 23, 24, 25))
#'   print(df)
#'
#'   df1 <- convert_integer64(df, "never")
#'   df2 <- convert_integer64(df, "if_fits")
#'   df3 <- convert_integer64(df, "if_summable")
#'   df4 <- convert_integer64(df, "always_quiet")
#'
#'   print(sapply(df,  class))
#'   print(sapply(df1, class))
#'   print(sapply(df2, class))
#'   print(sapply(df3, class))
#'   print(sapply(df4, class))
#'   
#'   print(df2)
#'   print(df4)
#'   
#'   cat("# Examples showing that integer64 is problematic:\n")
#'       y <- bit64::seq.integer64(1, 3)
#'       print(y)
#'       print(0.5 * y)
#'       print(y * 0.5)
#'       matrix(y, 1, 3)   
#' }
#'
convert_integer64 <- function(df,
                              to_integer = "if_fits",
                              precision_loss = "character",
                              always_character = FALSE) {
  
  
  # Allow input to be a single vector (not just a list or data.frame)
  df_is_list <- is.list(df)
  if (!df_is_list) {
    if (!inherits(df, "integer64")) {
      return(df)  # nothing to convert
    }
    df <- list(df)  # wrap in list for unified handling below
  }
  
  
  valid_opts <- c("never", "if_fits", "if_summable", "always", "always_quiet")
  if (!is.character(to_integer) || length(to_integer) != 1L || !to_integer %in% valid_opts) {
    stop(sprintf("convert_integer64: 'to_integer' must be one of: %s",
                 paste(valid_opts, collapse = ", ")))
  }
  
  valid_precision <- c("character", "warn", "quiet")
  if (!is.character(precision_loss) || length(precision_loss) != 1L ||
      !precision_loss %in% valid_precision) {
    stop(sprintf("convert_integer64: 'precision_loss' must be one of: %s",
                 paste(valid_precision, collapse = ", ")))
  }
  
  # Identify integer64 variables
  has_i64 <- any(sapply(df, inherits, "integer64"))
  if (!has_i64) {
    return(df)  # nothing to convert
  }
  
  # Require bit64 if conversion is needed
  if (!requireNamespace("bit64", quietly = TRUE)) {
    stop("convert_integer64: found 'integer64' variables, but the 'bit64' package is not installed.")
  }
  
  # Define helper for numeric conversion
  as_num <- function(x_i64) {
    if (precision_loss == "warn") {
      return(as.numeric(x_i64))
    }
    
    if (precision_loss == "quiet") {
      return(suppressWarnings(as.numeric(x_i64)))
    }
    
    # precision_loss == "character"
    th <- bit64::as.integer64("9007199254740991")  # 2^53 - 1
    outside <- (x_i64 > th) | (x_i64 < -th)
    need_char <- any(outside[!is.na(outside)])
    
    if (need_char) {
      as.character(x_i64)
    } else {
      suppressWarnings(as.numeric(x_i64))
    }
  }
  
  df[] <- lapply(df, function(x) {
    if (inherits(x, "integer64")) {
      
      if (always_character) {
        return(as.character(x))
      }
      
      # Convert the integer64 column to either numeric or character,
      # depending on the precision_loss setting.
      x <- as_num(x)
      
      # Decide whether to convert to integer, depending on to_integer mode.
      convert <- switch(
        to_integer,
        never = FALSE,
        always = TRUE,
        always_quiet = TRUE,
        if_fits = {
          # Only perform the range check if x is numeric.
          # Prevents errors if precision_loss = "character" produced character values.
          if (is.numeric(x)) {
            all(is.na(x) | (is.finite(x) & abs(x) <= .Machine$integer.max))
          } else {
            FALSE
          }
        },
        if_summable = {
          # Only perform the summability check if x is numeric.
          # Protects against non-numeric input (e.g. from precision_loss = "character").
          if (is.numeric(x)) {
            s <- sum(abs(x), na.rm = TRUE)
            is.finite(s) && s < .Machine$integer.max
          } else {
            FALSE
          }
        }
      )
      
      # If conversion to integer is requested, force it.
      # as.integer() will handle both numeric and character input;
      if (isTRUE(convert)) {
        if (to_integer == "always_quiet") {
          x <- suppressWarnings(as.integer(x))
        } else {
          x <- as.integer(x)
        }
      }
    }
    
    # Return processed column
    x
  })
  
  # Unwrap if input was a single vector
  if (!df_is_list) {
    return(df[[1]])
  }
  
  df
}





