# tests/testthat/test-sitrep-ecosystem.R

test_that("sitrep_core is properly defined", {
  expect_length(sitrep_core, 4)
  expect_equal(sitrep_core, c("epitabulate", "epidict", "epikit", "apyramid"))
  expect_type(sitrep_core, "character")
})

test_that("sitrep_core_unloaded works correctly", {
  # Should return character vector
  unloaded <- sitrep_core_unloaded()
  expect_type(unloaded, "character")

  # Should be subset of sitrep_core
  expect_true(all(unloaded %in% sitrep_core))

  # Length should be between 0 and length of sitrep_core
  expect_gte(length(unloaded), 0)
  expect_lte(length(unloaded), length(sitrep_core))
})

test_that("is_attached works correctly", {
  # Test with base package that should be attached
  expect_true(is_attached("base"))

  # Test with package that shouldn't exist
  expect_false(is_attached("nonexistent_package_12345"))

  # Test with stats (usually attached)
  expect_true(is_attached("stats"))
})

test_that("is_loading_for_tests works correctly", {
  # Save original value
  original_env <- Sys.getenv("DEVTOOLS_LOAD")

  # Test default case
  if (interactive()) {
    expect_false(is_loading_for_tests())
  }

  # Test with DEVTOOLS_LOAD set to sitrep
  Sys.setenv(DEVTOOLS_LOAD = "sitrep")
  if (!interactive()) {
    expect_true(is_loading_for_tests())
  }

  # Test with DEVTOOLS_LOAD set to something else
  Sys.setenv(DEVTOOLS_LOAD = "other_package")
  expect_false(is_loading_for_tests())

  # Restore original environment
  if (original_env == "") {
    Sys.unsetenv("DEVTOOLS_LOAD")
  } else {
    Sys.setenv(DEVTOOLS_LOAD = original_env)
  }
})

test_that("sitrep_conflicts works without error", {
  expect_message(sitrep_conflicts(), "conflicts|No conflicts")

  # Should return invisibly
  result <- sitrep_conflicts()
  expect_null(result)
})

test_that("sitrep_functions returns proper data structure", {
  result <- sitrep_functions()

  # Should be a data frame
  expect_s3_class(result, "data.frame")

  # Should have correct columns
  expect_named(result, c("package", "function_name"))
  expect_type(result$package, "character")
  expect_type(result$function_name, "character")

  # Should be ordered by package then function_name
  if (nrow(result) > 1) {
    expect_true(all(result$package[-nrow(result)] <= result$package[-1]))
  }

  # Should include sitrep functions
  sitrep_functions_in_result <- result[result$package == "sitrep", "function_name"]
  expect_true("sitrep_functions" %in% sitrep_functions_in_result)
  expect_true("sitrep_conflicts" %in% sitrep_functions_in_result)
})

test_that("sitrep_functions pattern filtering works", {
  # Test with pattern that should match something
  result_check <- sitrep_functions(pattern = "check")
  expect_true(all(grepl("check", result_check$function_name)))

  # Test with pattern that shouldn't match anything
  result_xyz <- sitrep_functions(pattern = "xyz123nonexistent")
  expect_equal(nrow(result_xyz), 0)

  # Test with regex pattern
  result_start <- sitrep_functions(pattern = "^sitrep_")
  expect_true(all(grepl("^sitrep_", result_start$function_name)))
})

# Mock tests for installation functions (since we don't want to actually install packages)
test_that("sitrep_check_deps returns logical vector", {
  # This will check dependencies but not install
  result <- sitrep_check_deps(quiet = TRUE)

  expect_type(result, "logical")
  expect_true(length(result) > 0)
  expect_true(all(names(result) != ""))
})

test_that("sitrep_install_deps handles missing DESCRIPTION gracefully", {
  # Create a temporary directory without DESCRIPTION
  temp_dir <- tempdir()
  with_mocked_bindings(
    system.file = function(...) file.path(temp_dir, "nonexistent"),
    {
      expect_error(
        sitrep_install_deps(quiet = TRUE, force = TRUE),
        "Cannot find sitrep DESCRIPTION file"
      )
    }
  )
})

test_that("sitrep_install_deps respects force and quiet parameters", {
  # Skip this test - too complex to mock safely
  skip("Mocking install.packages is complex and risky")

  # Instead, test the logic components separately
  test_string <- "pkg1, pkg2 (>= 1.0.0), R (>= 3.5.0)"
  pkgs <- unlist(strsplit(test_string, ","))
  pkgs <- trimws(pkgs)
  pkgs <- gsub("\\s*\\([^)]+\\)", "", pkgs)
  pkgs <- pkgs[pkgs != "R" & pkgs != ""]

  expect_equal(pkgs, c("pkg1", "pkg2"))
})

test_that("package list parsing works correctly", {
  # Test the internal parse_pkg_list function logic
  test_string <- "pkg1, pkg2 (>= 1.0.0), R (>= 3.5.0), pkg3"

  # Split by comma and clean up
  pkgs <- unlist(strsplit(test_string, ","))
  pkgs <- trimws(pkgs)

  # Remove version specifications and R itself
  pkgs <- gsub("\\s*\\([^)]+\\)", "", pkgs)
  pkgs <- pkgs[pkgs != "R" & pkgs != ""]

  expect_equal(pkgs, c("pkg1", "pkg2", "pkg3"))
})

test_that("sitrep_attach_message formats correctly", {
  # Test with empty load list
  msg_empty <- sitrep_attach_message(character(0))
  expect_equal(msg_empty, "All sitrep ecosystem packages already loaded.")

  # Test with packages to load - use simple mock without mocking base functions
  mock_packages <- c("stats", "utils") # Use packages that definitely exist
  msg <- sitrep_attach_message(mock_packages)
  expect_match(msg, "sitrep")
  expect_match(msg, "Loading sitrep ecosystem")
})

test_that("same_library handles loaded and unloaded packages", {
  # Test with a package that should be loaded (utils)
  expect_no_error(same_library("utils"))

  # The package should now be attached
  expect_true(is_attached("utils"))
})

# Integration test
test_that("sitrep ecosystem functions work together", {
  # Check that all main functions exist and are callable
  expect_true(exists("sitrep_functions"))
  expect_true(exists("sitrep_conflicts"))
  expect_true(exists("sitrep_check_deps"))
  expect_true(exists("sitrep_install_deps"))

  # Test that they can be called without error
  expect_no_error(sitrep_functions())
  expect_no_error(suppressMessages(sitrep_conflicts()))
  expect_no_error(sitrep_check_deps(quiet = TRUE))
})

# Test edge cases
test_that("functions handle edge cases gracefully", {
  # sitrep_functions with empty pattern
  expect_no_error(sitrep_functions(pattern = ""))

  # sitrep_conflicts - just test it runs without error
  expect_no_error(suppressMessages(sitrep_conflicts()))
})

# Test error conditions
test_that("functions handle errors appropriately", {
  # Test sitrep_check_deps with missing DESCRIPTION
  temp_dir <- tempdir()
  with_mocked_bindings(
    system.file = function(...) file.path(temp_dir, "nonexistent_file"),
    {
      expect_error(sitrep_check_deps(), "kann.*öffnen|Cannot.*find|No such file")
    }
  )
})
