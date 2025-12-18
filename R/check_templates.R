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
#' @examples
#' \donttest{
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
    if (!quiet) {message(sprintf("Building %s", i))}

    res[[i]] <- build_sitrep_template(
      template = i,
      path = path,
      progress = progress,
      output_format = output_format,
      clean = clean)
  }

  # identify templates which errors
  has_error <- vapply(res, inherits, logical(1), "error")

  if (any(has_error)) {

    error_templates <- names(res)[has_error]

    error_msgs <- paste0(
      "Errors were found in the following templates:\n",
      paste0("  - ", error_templates, ": ",
             vapply(res[error_templates], conditionMessage, character(1)),
             collapse = "\n")

    )

    message(error_msgs)

    if (mustwork) {
      stop(error_msgs, call. = FALSE)
    }
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
    p <- suppressWarnings(sessioninfo::session_info()$package)
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

#' Draft and render a sitrep template
#'
#' This internal helper function drafts an R Markdown template from the
#' *sitrep* package into a specified directory and renders it using
#' **rmarkdown**. It optionally removes previously rendered output before
#' building.
#'
#' @param template A character string giving the name of the sitrep template
#'   to draft and render. This can be the name of a template included in the
#'   package, or a path to a user-provided template.
#' @param path A directory in which the drafted `.Rmd` file and rendered output
#'   will be saved.
#' @param progress Logical; if `TRUE`, progress messages from
#'   [rmarkdown::render()] are shown. Defaults to `FALSE`.
#' @param output_format A valid output format object to be passed to
#'   [rmarkdown::render()]. If `NULL` (default), the format defined in the
#'   template is used.
#' @param clean Logical; if `TRUE` (default), remove any previously rendered
#'   output file for this template before rendering.
#'
#' @return Returns the result of [rmarkdown::render()] if rendering succeeds.
#'   If an error occurs during drafting or rendering, the caught error
#'   condition is returned instead.
#'
#' @keywords internal
#' @noRd
build_sitrep_template <- function(template, path, progress = FALSE, output_format = NULL, clean = TRUE) {

  path_to_template <- file.path(path, sprintf("%s.Rmd", basename(template)))
  if (clean) {
    tmplt <- basename(template)
    fmt   <- if (is.null(output_format)) "docx" else output_format$pandoc$to
    output_file <- file.path(path, sprintf("%s.%s", tmplt, fmt))

    # Only try to remove if file exists
    if (file.exists(output_file)) {
      file.remove(output_file)
    }
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
