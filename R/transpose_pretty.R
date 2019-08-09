#' transpose the output of tabulate_binary_survey
#'
#' @param x a transposable data frame with a suffix for columns
#' @param columns the new name for the column in which to place the columns
#' @param rows the name of the rows
#' @param suffix the suffix of the columns to transpose
#' @param clev the levels for the stratifying columns. If this is NULL, that 
#'   means that there is no stratifying column and the result should be a single
#'   row. 
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
  #   4. make sure the columns are factored in order
  #   4. spread out the data so that the rows are now the columns

  res <- dplyr::select(x, !! var, dplyr::ends_with(sfx_char))
  res <- tidyr::gather(res, !! col, !! sfx, - !! var)
  res <- dplyr::mutate(res, !! col := gsub(sfx_char, "", !! col))
  res <- dplyr::mutate(res, !! var := forcats::fct_inorder(!!var))
  if (!is.null(clev)) {
    res <- dplyr::mutate(res, !! col := factor(!! col, clev))
  }
  res <- tidyr::spread(res, !! var, !! sfx)
  names(res)[-1] <- paste0(names(res)[-1], sfx_char)
  res
}

#' flip multiple columns of a table
#'
#' @param x survey data (TODO: CHANGE THIS)
#' @param res the resulting table with things to flip
#' @param transpose transpose by which columns
#' @param pretty is the table already pretty (gives ci or proportion columns)
#' @param stra the quoted strata variable (TODO: CHANGE THIS)
#' @param is_survey a logical indicating whether or not x is a survey
#'
#' @return a flipped data frame where the stratifying variables are flipped
#' @noRd
#'
#' @examples
#' 
flipper <- function(x, res, transpose = c("variable", "value", "both"), pretty = TRUE, stra, is_survey = TRUE) {

  transpose <- match.arg(tolower(transpose), c("variable", "value", "both"))

  if (transpose == "both") {
    # if the user wants to keep both columns, then we unite them and then
    res <- tidyr::unite(res, col = "both", "variable", "value", remove = TRUE)
  }

  # number of rows in the original table
  nr   <- seq_len(nrow(res))
  rnames <- names(res)
  suffix <- c(
    include_suffix(rnames, "deff"),
    include_suffix(rnames, "prop"),
    include_suffix(rnames, "ci"),
    include_suffix(rnames, "proportion"),
    include_suffix(rnames, "proportion_low"),
    include_suffix(rnames, "proportion_upp"),
    NULL
  )
  # if deff exists in the table
  # deff <- any(grepl("deff$", names(res)))
  # number of new columns based on the number of rows
  # nc   <- 1L + deff + if (pretty) 1L else 3L
  nc <- length(suffix) + 1L
  # the variable column to be transposed
  var  <- rlang::ensym(transpose)

  # when there is no strata, we have to come up with a dummy variable
  strata_exists <- tidyselect::vars_select(colnames(x), !! stra)
  strata_exists <- length(strata_exists) > 0

  if (strata_exists) {
    slevels <- dplyr::pull(x, !! stra)
    slevels <- if (is.factor(slevels)) levels(slevels) else sort(slevels)
  } else {
    stra      <- paste0("__", as.integer(Sys.time()))
    stra      <- rlang::ensym(stra)
    old_names <- names(res)[names(res) != transpose]
    names(res)[names(res) != transpose] <- paste0(" ", old_names)
    slevels   <- NULL
  }

  # transposing the count variable, which is always there.
  tres <- transpose_pretty(res, !! stra, !! var, !! rlang::sym("n"), slevels)

  # determining the list of suffixes to run through when appending the columns
  # suffix <- c(
  #             if (deff) "deff" else NULL,
  #             if (pretty) "ci" else c("proportion", "proportion_low", "proportion_upp")
  # )

  # transposing and appending the columns
  for (i in suffix) {
    suff <- rlang::ensym(i)
    tmp  <- transpose_pretty(res, !! stra, !! var, !! suff, slevels)
    tres <- dplyr::bind_cols(tres, tmp[-1])
  }

  # re-ordering the columns so that they are grouped by the original row order
  res <- tres[c(1, order(rep(nr, nc)) + 1)]
  if (!strata_exists) {
    res <- dplyr::select(res, - !! stra)
  }
  res
}


include_suffix <- function(x, suffix) {

  if (any(grepl(glue::glue("(. | ?){suffix}$"), x))) suffix else NULL

}
