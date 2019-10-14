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
#' @importFrom glue glue
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' # get population counts based on proportion, unstratified
#' gen_population(groups = c(1, 2, 3, 4), 
#'                strata = NULL, 
#'                proportions = c(0.3, 0.2, 0.4, 0.1))
#'
#' # get population counts based on proportion, stratified
#' gen_population(groups = c(1, 2, 3, 4), 
#'                strata = c("a", "b"), 
#'                proportions = c(0.3, 0.2, 0.4, 0.1))
#'
#' # get population counts based on counts, unstratified
#' gen_population(groups = c(1, 2, 3, 4), 
#'                strata = NULL, 
#'                counts = c(20, 10, 30, 40))
#'
#' # get population counts based on counts, stratified
#' gen_population(groups = c(1, 2, 3, 4), 
#'                strata = c("a", "b"), 
#'                counts = c(20, 10, 30, 40))
#'
#' # get population counts based on counts, stratified - type out counts
#' # for each group and strata
#' gen_population(groups = c(1, 2, 3, 4), 
#'                strata = c("a", "b"), 
#'                counts = c(20, 10, 30, 40, 40, 30, 20, 20))


gen_population <- function(total_pop = 1000,
                           groups = c("0-4","5-14","15-29","30-44","45+"),
                           strata = c("Male", "Female"),
                           proportions = c(0.079, 0.134, 0.139, 0.082, 0.066),
                           counts = NULL,
                           tibble = TRUE) {

  # pick counts if not empty
  measure <- if (is.null(counts)) proportions else counts

  # define lengths of inputs for checks
  ngroups <- length(groups)
  nstrata <- length(strata)
  nmeasure <- length(measure)


  # if strata specified then make a dataframe from combining groups*strata
  if (!is.null(strata)) {

    # give error - check length of given input measure and groups match with strata
    if (nmeasure != ngroups && nmeasure != ngroups * nstrata) {

      differences <- abs(ngroups - nmeasure)
      differences2 <- abs(ngroups * nstrata - nmeasure)

      stop(glue::glue("Given proportions (or counts) and groups lengths",
                      "do not match, nor are they a multiple of strata\n",
                      "The difference in length was {differences}\n",
                      "The difference in multiplied length was {differences2}"))
    }

    # give warning if repeating input measures for strata
    if (nmeasure != ngroups * nstrata) {
      warning(glue::glue("Given proportions (or counts) is not the same as\n",
                         "groups multiplied by strata length, they will be repeated to match"))
    }

    # create data frame with groups and strata
    output <- expand.grid(groups = groups, strata = strata)
  } else {
    # check length of given input measure and groups match without strata
    if (nmeasure != ngroups) {
      differences <- abs(ngroups - nmeasure)

      stop(glue::glue("Given proportions (or counts) and groups lengths
                    do not match and there were no strata\n",
                      "The difference in length was {differences}"))
    }

    # define a dataframe based on groups only
    output <- data.frame(groups)
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
    # for counts
    output$n <- counts
    output$proportions <- output$n / sum(output$n)

    if (!is.null(strata)) {
      output <- output[ , c("groups", "strata", "proportions", "n")]
    } else {
      output <- output[ , c("groups", "proportions", "n")]
    }
  }


  if (tibble) {
    output <- tibble::as_tibble(output)
  }

  output

}
