#' Counts with proportions and totals table 
#' @import tidyr
#' @export

descriptive <- function(df, counter, grouper = NA, multiplier = 100, digits = 1){

  if (!is.na(grouper)) {
    count_data <- group_by_(df, grouper) %>% 
                    count_(counter) %>% 
                    mutate(prop = round(n/sum(n)*multiplier, digits = digits))
    
    count_data <- count_data %>% 
                    gather(variable, value, c(n, prop)) %>%
                    unite(temp, !!grouper, variable, sep = "_") %>%
                    spread(temp, value)
    
  } else{
    # get counts
    count_data <- count_(df, outcome)
    # get total 
    total <- sum(count_data$n)
    
    count_data <- count_data %>% 
                  rowwise() %>% 
                  mutate(prop = round(n / total*multiplier, digits = digits))

  }
  
  
  count_data
  
}

