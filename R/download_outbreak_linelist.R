#' @title download_outbreak_linelist
#'
#' Access AJS outbreak linelist
#'
#' @param path **Run with no arguments to pick a directory interactively**. Path on your computer where the file(s) should be saved to *string*
#'
#' @importFrom rstudioapi selectDirectory
#' @importFrom cli cli_alert_success
#'
#' @export
download_outbreak_linelist <- function(path = rstudioapi::selectDirectory()){

  ## normalise in case given manually
  path <- normalizePath(path)

  file_to_copy <- system.file(package = "sitrep", "extdata", "AJS_AmTiman.xlsx")

  success <- lapply(file_to_copy, file.copy, to = path, overwrite = TRUE)

  if(all(as.logical(success))) cli::cli_alert_success("File successfully saved here: {path}")

}
