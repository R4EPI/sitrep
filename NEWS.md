# sitrep 0.1.7

* Import {epibuffet}. This replaces the `tab_linelist()`, `tab_survey()`,
  and `tab_univariate()` funcitons. 
* Remove `outbreaks` from imports.

# sitrep 0.1.6

 * Preferrentially use `matchmaker::match_df()` over 
   `linelist::clean_variable_spelling()`.

# sitrep 0.1.5

* Import {epikit}. This replaces several small formatting functions like 
  `fmt_ci()` and `rename_redundant()`. It also replaces `age_categories()` and
  `attack_rate()` etc.

# sitrep 0.1.4

* Rename import of {msfdict} to {epidict}
* Remove duplicate excel files from {epidict}

# sitrep 0.1.3

* `gen_polygon()` has been moved back into this package as it did not really
  belong in {msfdict}. Note that this should not affect the user experience.
  (@zkamvar, #235)

# sitrep 0.1.2

* Import {msfdict}. The `msf_dict()`, `msf_dict_survey()`, `gen_data()` and
  `gen_polygon()` functions (and all associated internal functions) were moved
  to the {msfdict} package and re-exported (@zkamvar, #228)

# sitrep 0.1.1

* Import {apyramid}. Code from `plot_age_pyramid()` was moved to the {apyramid}
  package, which is currently on GitHub, but soon to be on CRAN (@zkamvar, #225)
* Removed erroneous emoji in comments that was causing errors in Windows
  installations (@zkamvar, #227)
* NOTE: this release on GitHub was not associated with a formal version change.

# sitrep 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* First official release of the {sitrep} package.
