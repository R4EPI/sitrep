#' Run the templates found in the sitrep package.
#'
#' @param templates a vector of templates to create and render
#' @param path a directory in which to store the templates
#' @param quiet if `FALSE` (default), a message will be printed for every
#'   template.
#' @param progress if `TRUE`, then the progress of the template is printed 
#'   to screen (passed to the `quiet` argument of [rmarkdown::render()]).
#'   Defaults to `FALSE`
#' @param mustwork if `TRUE`, then the templates must work for the function to
#'   succeed. Defaults to `FALSE`, which will simply print the errors. 
#' @param output_format a character defining the output formats to use for the
#'   template files. Defaults to the output_format defined in the templates
#'   (which is `word_document`), but can be modified to `html_document` for
#'   cross-platform cromulence checking.
#' @param clean if `TRUE` (default), this will remove the previous output file
#'   before rendering.
#' 
#' @return the path where the templates were built.
#' @export
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' check_sitrep_templates("mortality")
#' }
check_sitrep_templates <- function(templates = available_sitrep_templates(),
                                   path = tempdir(), 
                                   quiet = FALSE, 
                                   progress = FALSE, 
                                   mustwork = FALSE,
                                   output_format = NULL,
                                   clean = TRUE) {

  stopifnot(is.character(templates), length(templates) > 0)

  res <- vector(mode = "list", length = length(templates))
  names(res) <- templates
  for (i in templates) {
    if (!quiet) message(sprintf("Building %s", i))
    res[[i]] <- build_sitrep_template(i, path, progress, output_format, clean = clean)
  }
  if (mustwork && any(errs <- vapply(res, inherits, logical(1), "error"))) {
    errs <- paste(names(res)[errs], collapse = ", ")
    message(sprintf("Errors were found in the following templates: %s", errs))
    return(res[errs])
  }
  return(path)
}


#' Display the available sitrep templates
#' 
#' @param categorise if `TRUE`, the results are split into a list of outbreak
#'   and survey categories. Defaults to `FALSE`. 
#' @param ... options to pass on to dir
#' @return a vector of available templates in the sitrep package
#'
#' @export
#'
#' @examples
#' available_sitrep_templates(categorise = TRUE)
#' available_sitrep_templates(categorise = TRUE, full.names = TRUE)
available_sitrep_templates <- function(categorise = FALSE, ...) {

  res <- dir(system.file("rmarkdown", "templates", package = "sitrep"), ...)

  if (requireNamespace("sessioninfo", quietly = TRUE)) {
    p <- sessioninfo::session_info()$package
    sr <- p$package == "sitrep"
    if (p$path[sr] != p$loadedpath[sr]) {
      res <- dir(file.path(p$loadedpath[sr], "inst", "rmarkdown", "templates"), ...)
    }
  } 

  if (categorise) {
    res <- split(res, ifelse(grepl("outbreak", res), "outbreak", "survey"))
  }

  res

}

# Draft and build a sitrep template (internal)
build_sitrep_template <- function(template, path, progress = FALSE, output_format = NULL, clean = TRUE) {

  path_to_template <- file.path(path, sprintf("%s.Rmd", basename(template)))
  if (clean) {
    tmplt <- basename(template)
    fmt   <- if (is.null(output_format)) "docx" else output_format$pandoc$to
    file.remove(file.path(path, sprintf("%s.%s", tmplt, fmt)))
  }
  res <- tryCatch({
    rmarkdown::draft(path_to_template,
                     template = template,
                     package = if (file.exists(template)) NULL else "sitrep",
                     edit = FALSE
                     )
    rmarkdown::render(path_to_template,
                      output_format = output_format,
                      output_dir = path,
                      encoding = "UTF-8",
                      quiet = !progress
                      )
  }, error = function(e) e)
  res

}
