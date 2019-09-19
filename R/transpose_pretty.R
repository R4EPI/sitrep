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

#' Convert the table to wide format consistently
#'
#' @param y a data frame
#' @param cod variable of interest
#' @param st stratifying variable
#' @noRd
widen_tabulation <- function(y, cod, st, pretty = TRUE, digits = 1) {

  # Setting up variables needed. Lower case are user-supplied
  cod   <- rlang::enquo(cod)
  st    <- rlang::enquo(st)

  # UPPER case variables are ones I create and destroy
  TIM   <- as.integer(Sys.time())
  KEY   <- rlang::sym(paste0("__key__",   TIM))
  VALUE <- rlang::sym(paste0("__value__", TIM))
  TMP   <- rlang::sym(paste0("__tmp__",   TIM))


  #  1 Only select the necessary columns. n, deff, and prop are all numeric
  #    columns that need to be gathered
  #
  #  2 Gather "n", "deff", and "prop" into a single column
  #
  #  3 Make sure that everything is arranged in the correct order. This will be
  #    arranged so that the rows are by the counter and then the stratifier.
  #
  #  4 Combine the stratifier and the n/prop signifier
  #
  #  5 Make sure the factors are in the correct order as "strata signifier"
  #
  #  6 Spread out the combined stratifier and signifier to columns
  #
  #  7 make sure the column arrangement matches the initial arrangement of the
  #    stratifier
  #  
  #  8 Run through the stratified columns with map, and make them pretty

  # getting all the labels for the stratifier
  l <- levels(dplyr::pull(y, !!st))
  # 1
  y <- dplyr::select(y, !!cod, !! st, "n",
                     # proportion and deff are all columns that _might_ exist
                     dplyr::matches("^proportion_?"), 
                     dplyr::matches("^deff$"))

  y <- tidyr::gather(y, key = !!KEY, value = !!VALUE, -(1:2))                 # 2
  y <- dplyr::arrange(y, !!cod, !! st)                                        # 3
  y <- tidyr::unite(y, !!TMP, !!st, !!KEY, sep = " ")                         # 4
  y <- dplyr::mutate(y, !!TMP := forcats::fct_inorder(dplyr::pull(y, !!TMP))) # 5
  y <- tidyr::spread(y, !!TMP, !!VALUE)                                       # 6

  # 7
  # guarding against common situation of having a stratifier that is 45+, changing to 45[+]
  protected_levels <- gsub("([[:punct:][:space:]])", "[\\1]", l)
  levels_in_order  <- glue::glue("^{protected_levels} (n|deff|proportion|ci)")
  col_arrangement  <- lapply(levels_in_order, grep, names(y))
  col_arrangement  <- unlist(col_arrangement, use.names = FALSE)
  y                <- y[c(1, col_arrangement)]

  if (pretty) {
    # map through all the levels of l and pull out the matching columns
    tmp <- purrr::map(l, ~dplyr::select(y, dplyr::starts_with(paste0(., " "))))
    # pretty up those columns and bind them all together
    tmp <- purrr::map2_dfc(tmp, l, ~prettify_tabulation(.x, digits = digits, ci_prefix = .y))

    # glue the result to the first column of the data
    y   <- dplyr::bind_cols(y[1], tmp)
  }
  
  return(y)

}
