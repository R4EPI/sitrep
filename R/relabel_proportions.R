#' Cosmetically relabel all columns that contains a certain pattern
#' 
#' These function are only to be used cosmetically before kable and will
#' likely return a data frame with duplicate names.
#'
#' - rename_redundant fully replaces any column names matching the keys
#' - augment_redundant will take a regular expression and rename columns 
#'     via [gsub()]. 
#' @param x a data frame
#' @param ... a series of keys and values to replace columns that match specific
#'   patterns.
#' @export
#' @author Zhian N. Kamvar
#' @examples
#'
#' df <- data.frame(x = letters[1:10], 
#'                  `a n` = 1:10, 
#'                  `a prop` = (1:10)/10,
#'                  `a deff` = round(pi, 2),
#'                  `b n` = 10:1,
#'                  `b prop` = (10:1)/10,
#'                  `b deff` = round(pi*2, 2),
#'                  check.names = FALSE)
#' df
#' print(df <- rename_redundant(df, "%" = "prop", "Design Effect" = "deff"))
#' print(df <- augment_redundant(df, " (n)" = " n$"))

rename_redundant <- function(x, ...) {

  pairs <- dots_to_charlist()
  
  for (i in seq_along(pairs)) {
    from <- pairs[[i]]
    to   <- names(pairs)[[i]]
    names(x)[grepl(from, names(x))] <- to
  }
  x

}

#' @rdname rename_redundant
#' @export
augment_redundant <- function(x, ...) {

  pairs <- dots_to_charlist()

  for (i in seq_along(pairs)) {
    from        <- pairs[[i]]
    to          <- names(pairs)[[i]]
    n           <- grepl(from, names(x))
    names(x)[n] <- gsub(from, to, names(x)[n])
  }
  x
}

#' Convert dots to a list of character vectors
#'
#' This function is intended to allow the user to use NSE within their dot
#' calls for the express purpose of renaming functions
#'
#' @param call a language object reflecting the current call. 
#' @param ... passed from the calling function 
#'   
#'
#' @return a list of character vectors
#' @noRd
#'
#' @examples
#' x <- function(...) {
#'   dots_to_charlist()
#' }
#' x(a = 1, b = TRUE, c = three)
dots_to_charlist <- function(parent = 1L) {
  sp <- sys.parent(n = parent)
  if (sp == 0) {
    stop('dots_to_charlist() can only be called within a user-facing function')
  }
  pairs <- match.call(definition  = sys.function(sp),
                      call        = sys.call(sp),
                      expand.dots = FALSE,
                      envir       = parent.frame(parent + 1L))[["..."]]
  pairs <- lapply(pairs, as.character)
  pairs
}
