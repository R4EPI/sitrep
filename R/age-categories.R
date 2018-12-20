#' Create an age group variable
#' either give distinct breaks or specify vals for sequence
#' @export


age_categories <- function(x, breakers = NA, 
                           lower = 0, upper = NA, 
                           by = 10,
                           separator = "-", 
                           above.char = "+") {
  
  
  # make sure age variable is numeric
  x <- as.numeric(x)
  
  # for specified breaks 
  if (!is.na(breakers[1])) {
    
    # create labels for groups 
    labs <- c(paste(breakers[-length(breakers)], # lower values
                    breakers[-1] - 1,  # higher values
                    sep = separator), # separator 
              paste(breakers[length(breakers)], 
                    above.char, sep = "") # highest grp
    ) 
    # store output var
    output <- cut(x, breaks = c(breakers, Inf), 
                  right = FALSE, labels = labs)
  }
  
  else{
    # create labels for groups
    labs <- c(paste(seq(lower, upper - by, by = by), # lower values
                    seq(lower + by - 1, upper - 1, by = by), # higher values
                    sep = separator), # separator
              paste(upper, above.char, sep = "") # highest group
    )
    # store output var
    output <- cut(x, breaks = c(seq(lower, upper, by = by), Inf),
                  right = FALSE, labels = labs)
  }
  
  # return variable with groups
  output
}
