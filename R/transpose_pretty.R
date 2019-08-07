#' transpose the output of tabulate_binary_survey
#'
#' @param x a transposable data frame with a suffix for columns
#' @param columns the new name for the column in which to place the columns
#' @param rows the name of the rows
#' @param suffix the suffix of the columns to transpose
#'
#' @noRd
#'
transpose_pretty <- function(x, columns, rows, suffix, clev = NULL) {
  col      <- rlang::enquo(columns)
  var      <- rlang::enquo(rows)
  sfx      <- rlang::enquo(suffix)
  sfx_char <- paste0(" ", rlang::as_name(sfx))

  # Strategy: 
  #   1. select only the variables that matter
  #   2. gather in the long format
  #   3. remove the suffix
  #   4. spread out the data so that the rows are now the columns

  res <- dplyr::select(x,   !! var, dplyr::ends_with(sfx_char))
  res <- tidyr::gather(res, !! col, !! sfx, - !! var)
  res <- dplyr::mutate(res, !! col := gsub(sfx_char, "", !! col))
  if (!is.null(clev)) {
    res <- dplyr::mutate(res, !! col := factor(!! col, clev))
  }
  res <- tidyr::spread(res, !! var, !! sfx)
  names(res)[-1] <- paste0(names(res)[-1], sfx_char)
  res
}
