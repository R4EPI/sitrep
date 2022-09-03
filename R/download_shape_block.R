#' @title download_shape_block
#'
#' Access Am Timan blocks shapefile
#'
#' @param path **Run with no arguments to pick a directory interactively**. Path on your computer where the file(s) should be saved to *string*
#'
#' @export
download_shape_block <- function(path = rstudioapi::selectDirectory()){

  ## normalise in case given manually
  path <- normalizePath(path)

  file_to_copy <- system.file(package = "sitrep", "extdata", "Blocksshape.shp")

  success <- lapply(file_to_copy, file.copy, to = path, overwrite = TRUE)

  if(all(as.logical(success))) cli::cli_alert_success("File successfully saved here: {path}")

}
