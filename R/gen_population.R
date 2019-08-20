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
#'   for each group of the total_pop.
#'   The default repeats c(0.079, 0.134, 0.139, 0.082, 0.067)
#'   for strata.  However you can change this manually, make sure to
#'   have the length equal to groups times strata (or half thereof).
#'   These defaults are based of MSF general emergency intervention standard
#'   values.
#' @param counts A numeric vector specifying the counts for each group.
#'   The default is NULL - as most often proportions above will be used.
#'   If is not NULL then total_pop and proportions will be ignored.
#'   Make sure the length of this vector is equal to groups times strata (or if it
#'   is half then it will repeat for each strata).
#'   For reference, the MSF general emergency intervention standard values
#'   are c(7945, 13391, 13861, 8138, 6665) based on above
#'   groups for a 100,000 person population.
#' @param tibble Return data as a tidyverse tibble (default is TRUE)
#' @importFrom dplyr bind_cols
#' @export
#'
#' @examples
#' # get population counts based on proportion, unstratified
#' gen_population(groups = c(1, 2, 3, 4), strata = NULL, proportions = c(0.3, 0.2, 0.4, 0.1))
#'
#' # get population counts based on proportion, stratified
#' gen_population(groups = c(1, 2, 3, 4), strata = c("a", "b"), proportions = c(0.3, 0.2, 0.4, 0.1))
#'
#' # get population counts based on counts, unstratified
#' gen_population(groups = c(1, 2, 3, 4), strata = NULL, counts = c(20, 10, 30, 40))
#'
#' # get population counts based on counts, stratified
#' gen_population(groups = c(1, 2, 3, 4), strata = c("a", "b"), counts = c(20, 10, 30, 40))
#'


gen_population <- function(total_pop = 1000,
                           groups = c("0-4","5-14","15-29","30-44","45+"),
                           strata = c("Male", "Female"),
                           proportions = c(0.079, 0.134, 0.139, 0.082, 0.066),
                           counts = NULL,
                           tibble = TRUE) {

  # measure <- ifelse(is.null(counts), proportions, counts)

  # # check length of given input measure and groups match without strata
  # if (is.null(strata) & length(measure) != length(groups)) {
  #   differences <- abs(length(groups) - length(measure))
  #
  #   stop(glue::glue("Given proportions (or counts) and groups lengths
  #                   do not match and there were no strata\n",
  #                   "The difference in length was {differences}"))
  # }
  #
  # # check length of given input measure and groups match with strata
  # if (!is.null(strata) &
  #     (length(measure) != length(groups) & length(measure) != length(groups) * length(strata))) {
  #
  #   differences <- abs(length(groups) - length(measure))
  #   differences2 <- abs(length(groups)*length(strata) - length(measure))
  #
  #   stop(glue::glue("Given proportions (or counts) and groups lengths
  #                   do not match, nor are they a multiple of strata\n",
  #                   "The difference in length was {differences}\n",
  #                   "The difference in multiplied length was {differences2}"))
  # }
  #
  #
  # # give warning if repeating input measures for strata
  # if (!is.null(strata) &
  #     length(measure) != length(groups) * length(strata)) {
  #   warning(glue::glue("Given proportions (or counts) is not the same as \n",
  #           "groups multiplied by strata length, they will be repeated to match"))
  # }



  output <- data.frame(groups)

  # if strata specified then make a dataframe from combining groups*strata
  if (!is.null(strata)) {
  # create data frame with groups and strata
  output <- expand.grid(groups, strata)

  # rename columns appropriately
  colnames(output) <- c("groups", "strata")
  }


  # for proportions
  if (is.null(counts)) {
    # pull together the props
    output$proportions <- proportions

    # create counts based on multiplying by total population
    output$n <- output$proportions * total_pop

    # give warning if proportions are more than 5% away from 100%
    if (sum(output$proportions) > 1.05 | sum(output$proportions) < 0.95) {
      warning(glue::glue("The sum of given proportions is more than 5% away from 100% \n",
                         "If the intention was to provide proportions by strata then ignore this message"))
    }

  } else {
    output$n <- counts
    output$proportions <- output$n / sum(output$n)

    if (!is.null(strata)) {
      output <- output[ , c("groups", "strata", "proportions", "n")]
    } else {
      output <- output[ , c("groups", "proportions", "n")]
    }
  }


  # # using proportions (where counts not specified)
  # if (is.null(counts)) {
  #
  #   if (length(proportions) != length(groups) & is.null(strata)) {
  #     difference <- abs(length(groups) - length(proportions))
  #
  #     stop(sprintf("Proporitons and groups do not match, without specifying strata.
  #                     The difference in length was %d (proportions may have been dropped)",
  #                     difference))
  #
  #     proportions <- proportions[1:length(groups)]
  #
  #   }
  #
  #   if (sum(proportions) != 1) {
  #     warning(sprintf("Proportions do not add up to 1. Consider checking these"))
  #   }
  #
  #
  #   if (!is.null(strata)) {
  #
  #     strata2 <- factor(
  #                       rep.int(strata, length(groups)),
  #                       levels = strata
  #                       )
  #     strata2 <- sort(strata2)
  #
  #
  #     groups2 <-  factor(
  #                        rep.int(groups, length(strata)),
  #                        levels = groups
  #                        )
  #
  #     if (length(proportions) < length(groups2)) {
  #       proportions2 <- rep.int(proportions, length(strata))
  #     }
  #
  #     output <- bind_cols(groups = groups2,
  #                         strata = strata2,
  #                         proportions = proportions2,
  #                         n = proportions2 * total_pop)
  #
  #   } else {
  #     output <- bind_cols(groups = groups,
  #                         proportions = proportions,
  #                         n = proportions * total_pop)
  #   }
  # } else {
  #   # using counts
  #
  #   if (length(counts) != length(groups) & is.null(strata)) {
  #     difference <- abs(length(groups) - length(counts))
  #
  #     stop(sprintf("Counts and groups do not match, without specifying strata.
  #                     The difference in length was %d (counts may have been dropped)",
  #                     difference))
  #
  #     counts <- counts[1:length(groups)]
  #   }
  #
  #
  #
  #
  #
  #
  #   if (!is.null(strata)) {
  #
  #     strata2 <- factor(
  #       rep.int(strata, length(groups)),
  #       levels = strata
  #     )
  #     strata2 <- sort(strata2)
  #
  #
  #     groups2 <-  factor(
  #       rep.int(groups, length(strata)),
  #       levels = groups
  #     )
  #
  #     if (length(counts) < length(groups2)) {
  #       counts2 <- rep.int(counts, length(strata))
  #     }
  #
  #     output <- bind_cols(groups = groups2,
  #                         strata = strata2,
  #                         proportions = counts2 / sum(counts2),
  #                         n = counts2)
  #
  #   } else {
  #     output <- bind_cols(groups = groups,
  #                         proportions = counts / sum(counts),
  #                         n = counts)
  #   }
  # }

  # if (tibble) {
  #   output <- tibble::tibble(output)
  # }

  output

}
