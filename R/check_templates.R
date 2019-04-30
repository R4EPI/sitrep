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
#' 
#' @return the path where the templates were built.
#' @export
#'
#' @keywords internal
#' @examples
#' check_sitrep_templates("mortality")
check_sitrep_templates <- function(templates = available_sitrep_templates(),
                                   path = tempdir(), 
                                   quiet = FALSE, 
                                   progress = FALSE, 
                                   mustwork = FALSE) {

  stopifnot(is.character(templates), length(templates) > 0)

  res <- vector(mode = "list", length = length(templates))
  names(res) <- templates
  for (i in templates) {
    if (!quiet) message(sprintf("Building %s", i))
    res[[i]] <- build_sitrep_template(i, path, progress)
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
#' @return a vector of available templates in the sitrep package
#'
#' @export
#'
#' @examples
#' available_sitrep_templates(categorise = TRUE)
available_sitrep_templates <- function(categorise = FALSE) {

  res <- dir(system.file("rmarkdown", "templates", package = "sitrep"))
  if (categorise) {
    res <- split(res, ifelse(grepl("outbreak", res), "outbreak", "survey"))
  }
  res

}

# Draft and build a sitrep template (internal)
build_sitrep_template <- function(template, path, progress = FALSE) {

  path_to_template <- file.path(path, sprintf("%s.Rmd", template))
  res <- tryCatch({
    rmarkdown::draft(path_to_template,
                     template = template,
                     package = "sitrep",
                     edit = FALSE
                     )
    rmarkdown::render(path_to_template,
                      output_dir = path,
                      encoding = "UTF-8",
                      quiet = !progress
                      )
  }, error = function(e) e)
  res

}
