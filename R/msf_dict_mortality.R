# don't export this function - only for internal use

#' @importFrom rio import
#' @importFrom epitrix clean_labels
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom tidyr fill spread
#' @importFrom dplyr mutate group_by row_number ungroup select recode
#' @importFrom tidyselect everything
msf_dict_mortality <- function(name = "MSF-mortality_survey-dict.xlsx",
                               tibble = TRUE) {

  # get excel file path (need to specify the file name)
  path <- system.file("extdata", name, package = "sitrep")

  # read in data set - pasting the disease name for sheet
  dat_dict <- rio::import(path, which = "Data Dictionary")

  # clean col names
  colnames(dat_dict) <- epitrix::clean_labels(colnames(dat_dict))

  # fill NA values with previous non-NA value, replace "." in codes and names
  dat_dict <- dat_dict %>%
    tidyr::fill(tidyselect::everything(), .direction = "down") %>%
    dplyr::mutate(choice_code = replace(choice_code, choice_code == ".", NA),
                  choice_name = replace(choice_name, choice_name == ".", NA))

  # minor tidying, e.g.: create "CodeX" assignments
  dat_dict <- dat_dict %>%
    dplyr::mutate(data_element_name = column_name,
                  data_element_shortname = column_name,
                  data_element_valuetype = gsub(pattern = "Question",
                                                replacement = "",
                                                x = type)) %>%
    dplyr::select(-column_name, - type) %>%
    dplyr::group_by(data_element_shortname) %>%
    dplyr::mutate(code = paste0("Code", dplyr::row_number())) %>%
    dplyr::ungroup()

  # transform dat_dict to wide format (like outbreak dictionary)
  dat_dict_wide <- dat_dict %>%
    dplyr::select(-choice_code) %>%
    tidyr::spread(code, choice_name)

  dat_dict_wide$data_element_valuetype <- dat_dict_wide$data_element_valuetype %>%
    dplyr::recode("Integer" = "INTEGER_POSITIVE",
                  "Binary" = "TEXT",
                  "ChoiceMulti" = "TEXT",
                  "Text" = "LONG_TEXT",
                  "Geo" = "LONG_TEXT", #not quite true, but might be ok for dummy data
                  "Date" = "DATE",
                  "Choice" = "TEXT",
                  "Number" = "INTEGER_POSITIVE" # not quite true, but might be ok for dummy data
                  )


  # clean future var names
  # excel names (data element shortname)
  # csv names (data_element_name)
  dat_dict_wide$data_element_shortname <- epitrix::clean_labels(dat_dict_wide$data_element_shortname)
  dat_dict_wide$data_element_name      <- epitrix::clean_labels(dat_dict_wide$data_element_name)

  dat_dict_wide <- data.frame(dat_dict_wide)

  # return a tibble
  if (tibble) {
    dat_dict_wide <- tibble::as_tibble(dat_dict_wide)
  }

  dat_dict_wide
}


