#' Generate dummy datasets
#' This function produces randomised datasets based on MSF data dictionaries.
#' @param disease Specify which disease you would like to use.
#' Currently supports "Cholera", "Measles" and "Meningitis".
#' @param varnames Specify name of column that contains varnames.
#' Currently default set to "Item".
#' (this can probably be deleted once dictionaries standardise)
#' @param numcases Specify the number of cases you want (default is 300)
#' @import rio
#' @import epitrix
#' @noRd

gen_data <- function(disease, varnames = "Item", numcases = 300) {
  # read in data set - pasting the disease name for sheet
  dat_dict <- rio::import(epidict(), which = paste0(disease, "_content"))
  # drop extra columns (keep after varnames column to retain contents of vars)
  dat_dict <- dat_dict[, grep(varnames, names(dat_dict)):length(names(dat_dict))]
  # use the var names as rows
  row.names(dat_dict) <- dat_dict[, varnames]
  # remove the var names column
  dat_dict <- dat_dict[, -1]
  # flip the dataset
  dat_dict <- data.frame(t(dat_dict))
  # remove rownames
  row.names(dat_dict) <- NULL
  # clean resulting labels
  colnames(dat_dict) <- epitrix::clean_labels(colnames(dat_dict))

  # define variables that do not have any contents in the data dictionary
  empties <- names(dat_dict[ , which(apply(!is.na(dat_dict), FUN = sum, MARGIN = 2) == 0)])

  # create an empty dataframe with the names from the data dictionary
  dis_output <- data.frame(matrix(ncol = ncol(dat_dict), nrow = numcases) )
  colnames(dis_output) <- colnames(dat_dict)

  # take samples for vars with defined options (non empties)
  dis_output[, !names(dat_dict) %in% empties] <- apply(dat_dict[, !names(dat_dict) %in% empties],
                                                       MARGIN = 2,
                                                       function(x) sample(x[!is.na(x)],
                                                                          numcases, replace = TRUE))

  datevars <- empties[grep("date", empties)]


}




# function to get excel file path (need to specify the file name)
epidict <- function(name = "outbreak-dict.xls") {
  system.file("extdata", name, package = "sitrep")
}
