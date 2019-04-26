#' Cosmetically relabel all columns that contains a certain pattern
#' 
#' This function is only to be used cosmetically before kable and will
#' likely return a data frame with duplicate names. This is a thin wrapper
#' around [dplyr::rename_at()]
#'
#' @param x a data frame
#' @param contains a character string that will contains all the columns whose names need
#'   to be replaced.
#' @param label the replacement label
#' @export
#' @author Zhian N. Kamvar
#' @examples
#'
#' df <- data.frame(x = letters[1:10], 
#'                  `a n` = 1:10, 
#'                  `a prop` = (1:10)/10,
#'                  `b n` = 10:1,
#'                  `b prop` = (10:1)/10,
#'                  check.names = FALSE)
#' rename_redundant(df)
rename_redundant <- function(x, contains = "prop", label = "% (95% CI)") {

  dplyr::rename_at(x, dplyr::vars(dplyr::contains(contains)),
                   ~function(i) rep(label, length(i)))

}
