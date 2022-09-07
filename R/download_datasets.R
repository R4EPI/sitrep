#' @title Internal function (not exported - i.e. not userfacing) to reduce code duplication
#'
#' @param path **Run with no arguments to pick a directory interactively**. Path on your computer where the file(s) should be saved to *string*
#' @param dataset The name of the file to save
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom cli cli_alert_success

generic_download <- function(path = rstudioapi::selectDirectory(), dataset){

  ## normalise in case given manually
  path <- normalizePath(path)

  file_to_copy <- system.file(package = "sitrep", "extdata", dataset)

  success <- lapply(file_to_copy, file.copy, to = path, overwrite = TRUE)

  if(all(as.logical(success))) cli::cli_alert_success("File successfully saved here: {path}")

}


#' @title Access kobo data dictionary
#'
#' @param path **Run with no arguments to pick a directory interactively**. Path on your computer where the file(s) should be saved to *string*
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom cli cli_alert_success
#'
#' @export
download_kobo <- function(path = rstudioapi::selectDirectory()){

  generic_download(path = path, dataset = "mortality_survey_dict.xlsx")

}


#' @title Access AJS outbreak linelist
#'
#' @param path **Run with no arguments to pick a directory interactively**. Path on your computer where the file(s) should be saved to *string*
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom cli cli_alert_success
#'
#' @export
download_outbreak_linelist <- function(path = rstudioapi::selectDirectory()){

  generic_download(path = path, dataset = "AJS_AmTiman.xlsx")

}


#' @title Access AJS outbreak population data
#'
#' @param path **Run with no arguments to pick a directory interactively**. Path on your computer where the file(s) should be saved to *string*
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom cli cli_alert_success
#'
#' @export
download_outbreak_pop <- function(path = rstudioapi::selectDirectory()){

  generic_download(path = path, dataset = "AJS_AmTiman_population.xlsx")

}


#' @title Access Am Timan blocks shapefile
#'
#' @param path **Run with no arguments to pick a directory interactively**. Path on your computer where the file(s) should be saved to *string*
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom cli cli_alert_success
#'
#' @export
download_shape_block <- function(path = rstudioapi::selectDirectory()){

  included_files <- list.files(system.file(package = "sitrep", "extdata"))

  dataset <- included_files[grep("Blocksshape", included_files)]

  generic_download(path = path, dataset = dataset)

}


#' @title Access Am Timan quartier shapefile
#'
#' @param path **Run with no arguments to pick a directory interactively**. Path on your computer where the file(s) should be saved to *string*
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom cli cli_alert_success
#'
#' @export
download_shape_quartier <- function(path = rstudioapi::selectDirectory()){

  included_files <- list.files(system.file(package = "sitrep", "extdata"))

  dataset <- included_files[grep("Quartiersshape", included_files)]

  generic_download(path = path, dataset = dataset)

}


#' @title Access fake mortality survey data
#'
#' @param path **Run with no arguments to pick a directory interactively**. Path on your computer where the file(s) should be saved to *string*
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom cli cli_alert_success
#'
#' @export
download_survey <- function(path = rstudioapi::selectDirectory()){

  generic_download(path = path, dataset = "mortality_survey_data.xlsx")

}
