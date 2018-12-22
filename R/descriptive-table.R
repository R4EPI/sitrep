#' Produces counts with respective proportions from specified variables in a dataframe. 
#' Option to add row and column totals 
#' @param df A dataframe (e.g. your linelist)
#' @param counter A name of the variable (in quotation marks) that you would like to have as rows. 
#' @param grouper A name of the variable (in quotation marks) that you would like to have as columns. 
#' @param multiplier What you would like to have your proportions as (default is per 100). 
#' @param digits The number of decimal places you would like in your proportions (default is 1). 
#' @param coltotals Add column totals on the end
#' @param rowtotals Add row totals (only sums counts)
#' @import tidyr
#' @export

descriptive <- function(df, counter, grouper = NA, multiplier = 100, digits = 1, 
                        coltotals = FALSE, rowtotals = FALSE){

  # if given two variables then group by the "grouper" var
  if (!is.na(grouper)) {
    # produce count table with props column-wise (seperate for each "grouper" level)
    count_data <- group_by_(df, grouper) %>% 
                    count_(counter) %>% 
                    mutate(prop = round(n/sum(n)*multiplier, digits = digits))
    
    # change to wide format, to have "grouper" var levels as columns
    count_data <- count_data %>% 
                    gather(variable, value, c(n, prop)) %>%
                    unite(temp, !!grouper, variable, sep = "_") %>%
                    spread(temp, value)
    
  } else{
    # get counts and props for just a single variable
    count_data <- count_(df, counter) %>% 
                  mutate(prop = round(n / sum(n)*multiplier, digits = digits))

  }
  
  # if there are NA counts, then change these to zero (except) in first col (which contains )
  count_data <- count_data %>%  mutate_at(2:ncol(.), funs(replace(., is.na(.), 0)))
  
  
  # if (coltotals == TRUE) {
  #   
  #   #make available to dplyr
  #   counter <- enquo(counter)
  #   
  #   count_data <- count_data %>%
  #     # change first column (with var levels) in to a character (for rbinding)
  #     ungroup() %>%
  #     mutate(!!counter := as.character(!!counter)) %>%
  #     # cbind a dataframe with "total and the column sums), rbind that to bottom
  #     rbind(cbind(!!counter := "Total",
  #                 t(round(colSums(count_data[,2:ncol(count_data)], na.rm = TRUE), digits = 0))))
  # }

  if (rowtotals == TRUE) {
    count_data <- count_data %>%
      # add columns which have "_n" in the name
      mutate(Total = rowSums(count_data[,grep("_n", colnames(count_data))], na.rm = TRUE))
  }

  
  count_data
  
}

