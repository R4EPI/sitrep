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
#' print(df <- rename_redundant(df, prop = "%", deff = "Design Effect"))
#' print(df <- augment_redundant(df, " n$" = "(n)"))

rename_redundant <- function(x, ...) {

  renames <- c(...)
  for (i in names(renames)) {
    names(x)[grepl(i, names(x))] <- renames[[i]]
  }
  x

}

#' @rdname rename_redundant
#' @export
augment_redundant <- function(x, ...) {

  augs <- c(...)
  for (i in names(augs)) {
    n           <- grepl(i, names(x))
    names(x)[n] <- gsub(i, augs[[i]], names(x)[n])
  }
  x
}
