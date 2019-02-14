#' Generate population counts from estimated population age breakdowns
#' This generates based on predefined age groups and proportions,
#' however you could also define these yourself.
#' @param total_pop The overal population count of interest - the default is 1000 people
#' @param age_groups A character vector of groups -
#' the default is set for age groups: c("0-4","5-14","15-24","25-34","35-44","45+")
#' @param proportions A numeric vector specifying the proportions for each age group -
#' the default is c(0.182, 0.278, 0.26, 0.11, 0.07, 0.10)
#' @param tibble Return data as a tidyverse tibble (default is TRUE)
#' @importFrom dplyr bind_cols
#' @export


gen_population <- function(total_pop = 1000,
                           groups = c("0-4","5-14","15-24","25-34","35-44","45+"),
                           proportions = c(0.182, 0.278, 0.26, 0.11, 0.07, 0.10),
                           tibble = TRUE) {

  output <- bind_cols(groups = groups,
                      proportions = proportions,
                      n = proportions*total_pop)

  if (tibble == FALSE) {
    output <- data.frame(output)
  }

  output

}
