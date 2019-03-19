# function to load MSF data dictionary for mortality surveys

#' @importFrom rio import
#' @importFrom epitrix clean_labels
#' @importFrom tibble as_tibble
#' @importFrom tidyr fill spread
#' @importFrom dplyr mutate group_by row_number ungroup
#' @export
#' @rdname msf_dict
msf_dict_mortality <- function(name = "MSF-mortality_survey-dict.xlsx",
                               tibble = TRUE) {

  # get excel file path (need to specify the file name)
  path <- system.file("extdata", name, package = "sitrep")

  # read in data set - pasting the disease name for sheet
  dat_dict <- rio::import(path, which = "Data Dictionary")

  # clean col names
  colnames(dat_dict) <- epitrix::clean_labels(colnames(dat_dict))

  # fill NA values with previous non-NA value, replace "." in codes and names
  dat_dict <- tidyr::fill(dat_dict, colnames(dat_dict), .direction = "down")

  # minor tidying, e.g.: create "CodeX" assignments
  dat_dict$choice_code[dat_dict$choice_code == "."] <- NA
  dat_dict$choice_name[dat_dict$choice_name == "."] <- NA
  dat_dict$data_element_valuetype <- gsub(pattern = "Question",
                                          replacement = "",
                                          x = dat_dict$type)
  dat_dict$type <- NULL
  dat_dict <- dplyr::group_by(dat_dict, .data$column_name)
  dat_dict <- dplyr::mutate(dat_dict,
                            code = paste0("Code", dplyr::row_number()))
  dat_dict <- dplyr::ungroup(dat_dict)
  dat_dict$code <- factor(dat_dict$code, unique(dat_dict$code))

  # transform dat_dict to wide format (like outbreak dictionary)
  dat_dict$choice_code <- NULL
  dat_dict_wide <- tidyr::spread(dat_dict, .data$code, .data$choice_name)

  dat_dict_wide$data_element_valuetype[dat_dict_wide$data_element_valuetype == "Integer"] <- "INTEGER_POSITIVE"
  dat_dict_wide$data_element_valuetype[dat_dict_wide$data_element_valuetype == "Binary"] <- "TEXT"
  dat_dict_wide$data_element_valuetype[dat_dict_wide$data_element_valuetype == "ChoiceMulti"] <- "MULTI"
  dat_dict_wide$data_element_valuetype[dat_dict_wide$data_element_valuetype == "Text"] <- "LONG_TEXT"
  dat_dict_wide$data_element_valuetype[dat_dict_wide$data_element_valuetype == "Geo"] <- "LONG_TEXT"
  dat_dict_wide$data_element_valuetype[dat_dict_wide$data_element_valuetype == "Date"] <- "DATE"
  dat_dict_wide$data_element_valuetype[dat_dict_wide$data_element_valuetype == "Choice"] <- "TEXT"
  dat_dict_wide$data_element_valuetype[dat_dict_wide$data_element_valuetype == "Number"] <- "INTEGER_POSITIVE"


  # clean future var names
  # excel names (data element shortname)
  # csv names (data_element_name)
  dat_dict_wide$column_name <- epitrix::clean_labels(dat_dict_wide$column_name)

  dat_dict_wide <- data.frame(dat_dict_wide, stringsAsFactors = FALSE)

  # return a tibble
  if (tibble) {
    dat_dict_wide <- tibble::as_tibble(dat_dict_wide)
  }

  dat_dict_wide
}


