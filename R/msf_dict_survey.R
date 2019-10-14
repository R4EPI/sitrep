# function to load MSF data dictionary for mortality surveys

#' @importFrom rio import
#' @importFrom epitrix clean_labels
#' @importFrom tibble as_tibble
#' @importFrom tidyr fill spread
#' @importFrom dplyr mutate group_by row_number ungroup
#' @export
#' @rdname msf_dict
msf_dict_survey <- function(disease, name = "MSF-survey-dict.xlsx",
                            tibble = TRUE,
                            compact = FALSE) {

  # get excel file path (need to specify the file name)
  path <- system.file("extdata", name, package = "sitrep")

  # read in data set - pasting the disease name for sheet
  dat_dict <- rio::import(path, which = disease)

  # clean col names
  colnames(dat_dict) <- epitrix::clean_labels(colnames(dat_dict))

  # fill NA values with previous non-NA value, replace "." in codes and names
  dat_dict <- tidyr::fill(dat_dict, colnames(dat_dict), .direction = "down")
  dat_dict <- dplyr::rename_at(dat_dict,
                               dplyr::vars(dplyr::starts_with("choice_")),
                               .funs = ~gsub("choice", "option", .)
                              ) 

  # minor tidying, e.g.: create "CodeX" assignments
  dat_dict$option_code[dat_dict$option_code == "."] <- NA
  dat_dict$option_name[dat_dict$option_name == "."] <- NA
  dat_dict$type <- gsub(pattern = "Question",
                        replacement = "",
                        x = dat_dict$type)
  # dat_dict <- dplyr::group_by(dat_dict, .data$column_name)
  # dat_dict <- dplyr::mutate(dat_dict,
  #                           code = paste0("Code", dplyr::row_number()))
  # dat_dict <- dplyr::ungroup(dat_dict)
  # dat_dict$code <- factor(dat_dict$code, unique(dat_dict$code))

  # transform dat_dict to wide format (like outbreak dictionary)
  dat_dict <- dplyr::select(dat_dict, 
                            !! quote(level),
                            !! quote(column_name),
                            !! quote(description), 
                            !! quote(type),
                            dplyr::starts_with("option_"))
  dat_dict <- dplyr::group_by(dat_dict, !! quote(column_name))

  dat_dict <- dplyr::mutate(dat_dict, option_order_in_set = seq(dplyr::n()))

  if (compact) {
    if (utils::packageVersion("tidyr") > "0.8.99") {
      dat_dict <- tidyr::nest(dat_dict, options = dplyr::starts_with("option_"))
    } else {
      squished <- tidyr::nest(dat_dict, dplyr::starts_with("option_"), .key = "options")
      dat_dict <- dplyr::select(dat_dict, -dplyr::starts_with("option_"))
      dat_dict <- dplyr::distinct(dat_dict)
      dat_dict <- dplyr::left_join(dat_dict, squished, by = "column_name")
    }
  }
  dat_dict <- dplyr::ungroup(dat_dict)
  

  dat_dict$type <- dplyr::case_when(
    dat_dict$type == "Integer" ~ "INTEGER_POSITIVE",
    dat_dict$type == "Binary" ~ "TEXT",
    dat_dict$type == "ChoiceMulti" ~ "MULTI",
    dat_dict$type == "Text" ~ "LONG_TEXT",
    dat_dict$type == "Geo" ~ "LONG_TEXT",
    dat_dict$type == "Date" ~ "DATE",
    dat_dict$type == "Choice" ~ "TEXT",
    dat_dict$type == "Number" ~ "INTEGER_POSITIVE"
  )

  dat_dict <- dplyr::rename(dat_dict, "data_element_valuetype" = "type")

  # clean future var names
  # excel names (data element shortname)
  # csv names (data_element_name)
  dat_dict$column_name <- epitrix::clean_labels(dat_dict$column_name)

  dat_dict <- data.frame(dat_dict, stringsAsFactors = FALSE)

  # return a tibble
  if (compact || tibble) {
    dat_dict <- tibble::as_tibble(dat_dict)
  }

  dat_dict
}


