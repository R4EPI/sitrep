#' Generate population counts from estimated population age breakdowns.
#'
#' This generates based on predefined age groups and proportions,
#' however you could also define these yourself.
#'
#' @param total_pop The overal population count of interest - the default is
#'   1000 people
#' @param groups A character vector of groups - the default is set for age
#' groups: c("0-4","5-14","15-29","30-44","45+")
#' @param strata A character vector for stratifying groups -
#'   the default is set for gender: c("Male", "Female")
#' @param proportions A numeric vector specifying the proportions (as decimals)
#'   for each age group - the default repeats c(0.158, 0.268, 0.277, 0.163, 0.133)
#'   for genders.  However you can change this manually, make sure to
#'   have the length equal to groups times strata.
#'   These defaults are based of MSF general emergency intervention standard
#'   values.
#' @param tibble Return data as a tidyverse tibble (default is TRUE)
#' @importFrom dplyr bind_cols
#' @export


gen_population <- function(total_pop = 1000,
                           groups = c("0-4","5-14","15-29","30-44","45+"),
                           strata = c("Male", "Female"),
                           proportions = c(0.158, 0.268, 0.277, 0.163, 0.133),
                           tibble = TRUE) {

  if (length(proportions) != length(groups) & is.null(strata)) {
    difference <- abs(length(groups) - length(proportions))

    warning(sprintf("Proporitons and groups do not match, without specifying strata.
                    The difference in length was %d (proportions may have been dropped)",
                    difference))

    proportions <- proportions[1:length(groups)]
  }


  if (!is.null(strata)) {

    strata2 <- factor(
                      rep.int(strata, length(groups)),
                      levels = strata
                      )
    strata2 <- sort(strata2)


    groups2 <-  factor(
                       rep.int(groups, length(strata)),
                       levels = groups
                       )

    if (length(proportions) < length(groups2)) {
      proportions2 <- rep.int(proportions, length(strata))
    }

    output <- bind_cols(groups = groups2,
                        strata = strata2,
                        proportions = proportions2,
                        n = proportions2 * total_pop)

  } else {
    output <- bind_cols(groups = groups,
                        proportions = proportions,
                        n = proportions * total_pop)
  }

  if (tibble == FALSE) {
    output <- data.frame(output)
  }

  output

}
