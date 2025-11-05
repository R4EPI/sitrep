# Functions to load and explore other functions in the sitrep ecosystem
# Inspired by the tidyverse metapackage

# Define core sitrep ecosystem packages
sitrep_core <- c(
  "epitabulate",
  "epidict",
  "epikit",
  "apyramid"
)

# Find packages that aren't already attached
sitrep_core_unloaded <- function() {
  search <- paste0("package:", sitrep_core)
  sitrep_core[!search %in% search()]
}

# Attach package from same library it was loaded from
# (prevents library path issues)
same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) {
    dirname(getNamespaceInfo(pkg, "path"))
  } else {
    NULL
  }

  suppressPackageStartupMessages(
    library(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
  )
}

# Main attach function
sitrep_attach <- function() {
  to_load <- sitrep_core_unloaded()

  # Only try to load packages that are available
  available_to_load <- to_load[vapply(to_load, requireNamespace, logical(1), quietly = TRUE)]

  if (length(available_to_load) > 0) {
    suppressMessages(
      lapply(available_to_load, same_library)
    )
  }

  invisible(available_to_load)
}

# Create startup message
sitrep_attach_message <- function(to_load) {
  if (length(to_load) == 0) {
    return("All sitrep ecosystem packages already loaded.")
  }

  sitrep_version <- utils::packageVersion("sitrep")

  # Header
  header <- paste0(
    "\nsitrep ", sitrep_version,
    " -- Report templates and helper functions for applied epidemiology\n",
    "Loading sitrep ecosystem:\n"
  )

  # Get versions for loaded packages
  to_load <- sort(to_load)
  versions <- vapply(to_load, function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      as.character(utils::packageVersion(pkg))
    } else {
      "not available"
    }
  }, character(1))

  # Format package list
  packages <- paste0("  \u2713 ", to_load, " ", versions)

  paste0(header, paste(packages, collapse = "\n"), "\n")
}

# Helper function to check if package is attached
is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

# Check if we're loading for tests (skip messages during testing)
is_loading_for_tests <- function() {
  !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "sitrep")
}

# Main .onAttach function
.onAttach <- function(libname, pkgname) {
  # Skip messages during testing
  if (is_loading_for_tests()) {
    return(invisible())
  }

  # Attach ecosystem packages
  attached <- sitrep_attach()

  # Show startup message
  packageStartupMessage(sitrep_attach_message(attached))
}

#' Check for function conflicts in sitrep ecosystem
#'
#' @description
#' This function identifies any naming conflicts between functions
#' in the sitrep ecosystem packages.
#'
#' @export
sitrep_conflicts <- function() {
  packages <- c("sitrep", sitrep_core)

  # Get all function names from each package
  all_functions <- list()
  for (pkg in packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      ns <- asNamespace(pkg)
      exported <- getNamespaceExports(ns)
      all_functions[[pkg]] <- exported
    }
  }

  # Find conflicts
  all_names <- unlist(all_functions)
  duplicated_names <- all_names[duplicated(all_names)]

  if (length(duplicated_names) == 0) {
    message("No conflicts found in sitrep ecosystem!")
    return(invisible())
  }

  # Show conflicts
  for (conflict_name in unique(duplicated_names)) {
    sources <- names(all_functions)[
      sapply(all_functions, function(x) conflict_name %in% x)
    ]
    message("Function '", conflict_name, "' found in: ", paste(sources, collapse = ", "))
  }
}

#' List all functions in sitrep ecosystem
#'
#' @description
#' Shows all available functions across the sitrep ecosystem packages.
#'
#' @param pattern Optional regex pattern to filter function names
#' @export
sitrep_functions <- function(pattern = NULL) {
  packages <- c("sitrep", sitrep_core)

  result <- data.frame(
    package = character(),
    function_name = character(),
    stringsAsFactors = FALSE
  )

  for (pkg in packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      ns <- asNamespace(pkg)
      exported <- getNamespaceExports(ns)

      if (!is.null(pattern)) {
        exported <- exported[grepl(pattern, exported)]
      }

      if (length(exported) > 0) {
        pkg_functions <- data.frame(
          package = pkg,
          function_name = exported,
          stringsAsFactors = FALSE
        )
        result <- rbind(result, pkg_functions)
      }
    }
  }

  result[order(result$package, result$function_name), ]
}


#' Install all sitrep ecosystem dependencies for offline use
#'
#' @description
#' This function installs all packages needed for the complete sitrep ecosystem,
#' ensuring you have everything needed for working without internet connection.
#'
#' @param upgrade Should packages be upgraded? Options: "ask", "always", "never". Default is "ask"
#' @param dependencies Should dependencies be installed? Default is TRUE
#' @param repos Repository to use. Default uses getOption("repos")
#' @param quiet Should installation be quiet? Default is FALSE
#' @param force Should installation proceed without prompts? Default is FALSE
#'
#' @details
#' This function reads the sitrep DESCRIPTION file and installs all packages
#' listed in Depends, Imports, and Suggests fields. This ensures complete
#' functionality in offline environments.
#'
#' @examples
#' \dontrun{
#' # Install everything for offline field work (with prompts)
#' sitrep_install_deps()
#'
#' # Install without prompts
#' sitrep_install_deps(force = TRUE)
#'
#' # Install quietly without prompts or upgrades
#' sitrep_install_deps(upgrade = "never", quiet = TRUE, force = TRUE)
#' }
#'
#' @export
sitrep_install_deps <- function(upgrade = "ask", dependencies = TRUE,
                                repos = getOption("repos"), quiet = FALSE,
                                force = FALSE) {

  # Get DESCRIPTION file path
  desc_path <- system.file("DESCRIPTION", package = "sitrep")
  if (!file.exists(desc_path)) {
    stop("Cannot find sitrep DESCRIPTION file")
  }

  # Read DESCRIPTION
  desc <- read.dcf(desc_path)

  # Extract package lists
  depends <- desc[, "Depends"]
  imports <- desc[, "Imports"]
  suggests <- desc[, "Suggests"]

  # Parse package names (remove version requirements and R itself)
  parse_pkg_list <- function(pkg_string) {
    if (is.na(pkg_string)) return(character(0))

    # Split by comma and clean up
    pkgs <- unlist(strsplit(pkg_string, ","))
    pkgs <- trimws(pkgs)

    # Remove version specifications and R itself
    pkgs <- gsub("\\s*\\([^)]+\\)", "", pkgs)
    pkgs <- pkgs[pkgs != "R" & pkgs != ""]
    pkgs
  }

  all_deps <- unique(c(
    parse_pkg_list(depends),
    parse_pkg_list(imports),
    parse_pkg_list(suggests)
  ))

  # Check which packages are missing
  installed <- vapply(all_deps, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  }, logical(1))

  missing <- all_deps[!installed]
  existing <- all_deps[installed]

  # Show current status
  if (!quiet) {
    cat("sitrep ecosystem package status:\n")
    cat("================================\n")
    if (length(existing) > 0) {
      cat("Already installed:", length(existing), "packages\n")
    }
    if (length(missing) > 0) {
      cat("Missing:", length(missing), "packages\n")
      cat("Missing packages:", paste(sort(missing), collapse = ", "), "\n")
    }
    cat("\n")
  }

  # If everything is installed, check if user wants upgrades
  if (length(missing) == 0 && upgrade == "never") {
    if (!quiet) {
      cat("All packages already installed and upgrade = 'never'. Nothing to do.\n")
    }
    return(invisible(all_deps))
  }

  # User confirmation (unless force = TRUE or running non-interactively)
  if (!force && interactive()) {
    if (length(missing) > 0) {
      cat("This will install", length(missing), "missing packages.\n")
    }

    if (length(existing) > 0 && upgrade %in% c("ask", "always")) {
      cat("This will also check for updates to", length(existing), "existing packages.\n")
    }

    cat("\nDo you want to proceed? [y/N]: ")
    response <- readline()

    if (!tolower(trimws(response)) %in% c("y", "yes")) {
      cat("Installation cancelled.\n")
      return(invisible(character(0)))
    }
  }

  # Show what we're about to do
  if (!quiet) {
    if (length(missing) > 0) {
      cat("Installing", length(missing), "missing packages...\n")
    }
    if (length(existing) > 0 && upgrade %in% c("ask", "always")) {
      cat("Checking for updates to", length(existing), "existing packages...\n")
    }
    cat("This may take several minutes...\n\n")
  }

  # Install packages
  if (length(all_deps) > 0) {
    utils::install.packages(
      all_deps,
      dependencies = dependencies,
      repos = repos,
      upgrade = upgrade,
      quiet = quiet
    )
  }

  if (!quiet) {
    cat("\nInstallation complete! sitrep ecosystem is ready for offline use.\n")
    cat("Run sitrep_check_deps() to verify all packages are working.\n")
  }

  invisible(all_deps)
}


#' Check if all sitrep dependencies are available
#'
#' @description
#' Checks if all packages needed for the sitrep ecosystem are installed
#' and loads properly. Useful for verifying offline readiness.
#'
#' @param quiet Should output be suppressed? Default is FALSE
#'
#' @return Logical vector indicating which packages are available
#'
#' @examples
#' \dontrun{
#' # Check what's missing before going offline
#' sitrep_check_deps()
#' }
#'
#' @export
sitrep_check_deps <- function(quiet = FALSE) {
  # Get all dependencies using same logic as install function
  desc_path <- system.file("DESCRIPTION", package = "sitrep")
  desc <- read.dcf(desc_path)

  parse_pkg_list <- function(pkg_string) {
    if (is.na(pkg_string)) return(character(0))
    pkgs <- unlist(strsplit(pkg_string, ","))
    pkgs <- trimws(pkgs)
    pkgs <- gsub("\\s*\\([^)]+\\)", "", pkgs)
    pkgs <- pkgs[pkgs != "R" & pkgs != ""]
    pkgs
  }

  all_deps <- unique(c(
    parse_pkg_list(desc[, "Depends"]),
    parse_pkg_list(desc[, "Imports"]),
    parse_pkg_list(desc[, "Suggests"])
  ))

  # Check availability
  available <- vapply(all_deps, requireNamespace, logical(1), quietly = TRUE)

  if (!quiet) {
    missing <- all_deps[!available]
    if (length(missing) == 0) {
      cat("All", length(all_deps), "sitrep ecosystem packages are available!\n")
      cat("Ready for offline use.\n")
    } else {
      cat(length(missing), "packages missing:\n")
      cat(paste(sort(missing), collapse = ", "), "\n\n")
      cat("Run sitrep_install_deps() to install missing packages.\n")
    }
  }

  invisible(available)
}


# Suppress R CMD check notes about unused imports
# (if you have packages in Imports that are only used by ecosystem packages)
ignore_unused_imports <- function() {
  # this list is created by running
  # blabla <- sitrep_functions()
  # clipr::write_clip(paste0(blabla$package, "::", blabla$function_name))

  apyramid::age_pyramid
  epidict::dict_rename_helper
  epidict::gen_data
  epidict::msf_dict
  # epidict::msf_dict_rename_helper
  epidict::read_dict
  epikit::add_weights_cluster
  epikit::add_weights_strata
  epikit::age_categories
  epikit::assert_positive_timespan
  epikit::augment_redundant
  epikit::constrain_dates
  epikit::dots_to_charlist
  epikit::fac_from_num
  epikit::find_breaks
  epikit::find_date_cause
  epikit::find_end_date
  epikit::find_start_date
  epikit::fmt_ci
  epikit::fmt_ci_df
  epikit::fmt_count
  epikit::fmt_pci
  epikit::fmt_pci_df
  epikit::gen_polygon
  epikit::gen_population
  epikit::group_age_categories
  epikit::merge_ci_df
  epikit::merge_pci_df
  epikit::rename_redundant
  epikit::unite_ci
  epikit::zcurve
  epitabulate::add_ar
  epitabulate::add_cfr
  epitabulate::add_crosstabs
  epitabulate::add_mr
  epitabulate::attack_rate
  epitabulate::case_fatality_rate
  epitabulate::case_fatality_rate_df
  epitabulate::gt_remove_stat
  epitabulate::mortality_rate
  epitabulate::tbl_cmh
}
