#' Counts with proportions and totals table 
#' @import tidyr
#' @export

descriptive <- function(df, variable, group = NA, multiplier = 100, digits = 1){

  if (!is.na(group)) {
    count_data <- group_by(df, df[,group]) %>% 
                    count(df[, variable]) %>% 
                    mutate(prop = round(n/sum(n)*multiplier, digits = digits) %>% 
                    # gather(variable, value, c(n, prop)) %>% 
                    # unite(temp, sex, variable, sep = "_") %>% 
                    # spread(temp, value)
  } else{
    # get counts
    count_data <- count(df, df[,variable])
    # get total 
    total <- sum(count_data$n)
    
    count_data <- count_data %>% 
                  rowwise() %>% 
                  mutate(prop = round(n / total*multiplier, digits = digits))
    
    # change var names
    colnames(count_data) <- c(variable, "n", "prop")

  }
  
  
  count_data
  
}

