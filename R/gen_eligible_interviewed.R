#' generate eligible and interviewed columns in a data frame
#'
#' @param dis_output a data frame containing household and cluster
#' @param household [character] the column specifying household
#' @param cluster [character] the column specifying cluster
#'
#' @return dis_output with two additional columns: 
#'   - eligible: the number of individuals within each household and cluster
#'   - interviewed: 75% of eligible
#'
#' @noRd
gen_eligible_interviewed <- function(dis_output, household = "q14_hh_no", cluster = "q77_what_is_the_cluster_number") {

  dis_output[["eligible"]] <- NULL
  dis_output[["interviewed"]] <- NULL

  hh <- rlang::sym(household)
  cl <- rlang::sym(cluster)

  # get counts of people by household and cluster
  hh_count <- dplyr::count(dis_output, !!hh, !!cl, .drop = FALSE, name = "eligible")

  # make interviewed 3/4s of those eligible
  hh_count[["interviewed"]] <- round(hh_count[["eligible"]] * 0.75, digits = 0L)

  # merge with dis_output and return
  dplyr::left_join(dis_output, hh_count, by = c(household, cluster))

}

