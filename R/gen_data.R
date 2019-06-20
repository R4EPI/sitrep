#' MSF data dictionaries and dummy datasets
#'
#' These function reads in MSF data dictionaries and produces randomised
#' datasets based on values defined in the dictionaries.  The randomised
#' dataset produced should mimic an excel export from DHIS2. 
#'
#' @param disease Specify which disease you would like to use.
#'   Currently supports "Cholera", "Measles" and "Meningitis".
#'
#' @param name the name of the dictionary stored in the package.
#'
#' @param dictionary Specify which dictionary you would like to use.
#'   Currently supports "Cholera", "Measles", "Meningitis", "AJS" and "Mortality".
#'
#' @param varnames Specify name of column that contains varnames. Currently
#'   default set to "Item".  (this can probably be deleted once dictionaries
#'   standardise) If `dictionary` is "Mortality", `varnames` needs to be "column_name"`.
#'
#' @param numcases For fake data, specify the number of cases you want (default is 300
#'
#' @param tibble Return data dictionary as a tidyverse tibble (default is TRUE)
#'
#' @param compact if `TRUE` (default), then a nested data frame is returned 
#'   where each row represents a single variable and a nested data frame column
#'   called "options", which can be expanded with [tidyr::unnest()]. This only
#'   works if `long = TRUE`.
#'
#' @param long If TRUE (default), the returned data dictionary is in long format with 
#'   each option getting one row. If `FALSE`, then two data frames are returned,
#'   one with variables and the other with content options. 
#'
#' @param copy_to_clipboard if `TRUE` (default), the rename template will be
#'   copied to the user's clipboard with [clipr::write_clip()]. If `FALSE`, the
#'   rename template will be printed to the user's console.
#'
#' @importFrom rio import
#' @importFrom epitrix clean_labels
#' @importFrom tibble as_tibble
#' @importFrom stats aggregate runif
#' @importFrom utils read.csv
#' @seealso [linelist::clean_variable_spelling()]
#' @export
#' @examples
#'
#' if (require('dplyr') & require('linelist')) { withAutoprint({
#' # You will often want to use MSF dictionaries to translate codes to human-
#' # readable variables. Here, we generate a data set of 20 cases:
#' dat <- gen_data(dictionary = "Cholera", varnames = "data_element_shortname", 
#'                 numcases = 20)
#' print(dat)
#' 
#' # We want the expanded dictionary, so we will select `compact = FALSE`
#' dict <- msf_dict(disease = "Cholera", long = TRUE, compact = FALSE, tibble = TRUE)
#' print(dict)
#'
#' # We can use linelist's clean_variable_spelling to translate the codes. First,
#' # we want to reorder the columns of the dictionary like so:
#' #
#' #  - 1st column: option codes
#' #  - 2nd column: translations
#' #  - 3rd column: data column name
#' #  - 4th column: order of options
#' # 
#' # we also want to make sure to filter out any columns that are blank for 
#' # the option codes, because this means that they don't have a fixed number of
#' # options
#' dict <- dict %>%
#'   select(option_code, option_name, data_element_shortname, option_order_in_set) %>%
#'   filter(!is.na(option_code))
#' print(dict)
#'
#' # Now we can use linelist to filter the data:
#' dat_clean <- clean_variable_spelling(dat, dict)
#' print(dat_clean)
#' })}


# function to pull outbreak data dicationaries together
msf_dict <- function(disease, name = "MSF-outbreak-dict.xlsx", tibble = TRUE,
                     compact = TRUE, long = TRUE) {

  # get excel file path (need to specify the file name)
  path <- system.file("extdata", name, package = "sitrep")

  # read in categorical variable content options
  dat_opts <- rio::import(path, which = "OptionCodes")

  # read in data set - pasting the disease name for sheet
  dat_dict <- rio::import(path, which = disease)

  # clean col names
  colnames(dat_dict) <- epitrix::clean_labels(colnames(dat_dict))
  colnames(dat_opts) <- epitrix::clean_labels(colnames(dat_opts))

  # clean future var names
  # excel names (data element shortname)
  # csv names (data_element_name)
  dat_dict$data_element_shortname <- epitrix::clean_labels(dat_dict$data_element_shortname)
  dat_dict$data_element_name      <- epitrix::clean_labels(dat_dict$data_element_name)

  # Adding hardcoded var types to options list
  # 2 types added to - BOOLEAN, TRUE_ONLY
  BOOLEAN   <- data.frame(option_code = c(1, 0),
                 option_name = c("[1] Yes", "[0] No"),
                 option_uid = c(NA, NA),
                 option_order_in_set = c(1,2),
                 optionset_uid = c("BOOLEAN", "BOOLEAN")
               )

  TRUE_ONLY <- data.frame(option_code = c(1, "NA"),
                 option_name = c("[1] TRUE", "[NA] Not TRUE"),
                 option_uid = c(NA, NA),
                 option_order_in_set = c(1,2),
                 optionset_uid = c("TRUE_ONLY", "TRUE_ONLY")
               )

  # bind these on to the bottom of dat_opts (option list) as rows
  dat_opts <- do.call("rbind", list(dat_opts, BOOLEAN, TRUE_ONLY))



  # add the unique identifier to link above three in dictionary to options list
  for (i in c("BOOLEAN", "TRUE_ONLY")) {
    dat_dict$used_optionset_uid[dat_dict$data_element_valuetype == i] <- i
  }

  # remove back end codes from front end var in the options list
  dat_opts$option_name <- gsub(".*] ", "", dat_opts$option_name)

  if (long) {
  
    outtie <- dplyr::left_join(dat_dict, dat_opts, 
                               by = c("used_optionset_uid" = "optionset_uid"))
    outtie <- if (tibble) tibble::as_tibble(outtie) else outtie
    
  }
  # produce clean compact data dictionary for use in gen_data
  if (long && compact == TRUE) {
    
    squished <- dplyr::group_by(outtie, !! quote(data_element_shortname))
    squished <- tidyr::nest(squished,  dplyr::starts_with("option_"), .key = "options")
    outtie   <- dplyr::select(outtie, -dplyr::starts_with("option_"))
    outtie   <- dplyr::distinct(outtie)
    outtie   <- dplyr::left_join(outtie, squished, by = "data_element_shortname")

    return(tibble::as_tibble(outtie))
    # change dat_opts to wide format
    # remove the optionset_UID for treatment_facility_site
    # (is just numbers 1:50 and dont want it in the data dictionary)
    # a <- stats::aggregate(option_code ~ optionset_uid,
    #                       dat_opts[dat_opts$optionset_uid != "MilOli6bHV0",], I) # spread wide based on UID
    # obs     <- lengths(a$option_code) # count length of var opts for each
    # highest <- seq_len(max(obs)) # create sequence for pulling out of list
    # out     <- t(sapply(a$option_code, "[", i = highest)) # pull out of list and flip to make dataframe
    # colnames(out) <- sprintf("Code%d", seq(ncol(out))) # rename with code and num of columns

    # # bind to above
    # a$option_code <- NULL
    # a <- cbind(a, out)

    # # repeat above for names
    # b <- stats::aggregate(option_name ~ optionset_uid,
    #                       dat_opts[dat_opts$optionset_uid != "MilOli6bHV0",], I) # spread wide based on UID
    # obs     <- lengths(b$option_name) # count length of var opts for each
    # highest <- seq_len(max(obs)) # create sequence for pulling out of list
    # out     <- t(sapply(b$option_name, "[", i = highest)) # pull out of list and flip to make dataframe
    # colnames(out) <- sprintf("Name%d", seq(ncol(out))) # rename with code and num of column

    # b$option_name <- NULL
    # b$optionset_uid <- NULL
    # b <- cbind(b, out)

    # # bind code and name together
    # combiner <- cbind(a, b)

    # # merge with data dicationry
    # outtie <- merge(dat_dict, combiner,
    #                 by.x = "used_optionset_uid", by.y = "optionset_uid",
    #                 all.x = TRUE)

    # # return a tibble
    # if (tibble == TRUE) {
    #   outtie <- tibble::as_tibble(outtie)
    # }

  }

  # Return second option: a list with data dictionary and value options seperate
  if (!long) {

    if (tibble == TRUE) {
      outtie <- list(dictionary = tibble::as_tibble(dat_dict),
                     options = tibble::as_tibble(dat_opts)
                     )
    }

    if (tibble == FALSE) {
      outtie <- list(dictionary = dat_dict,
                     options = dat_opts)
    }

  }

  # return dictionary dataset
  outtie
}

#' @export
#' @rdname msf_dict
msf_dict_rename_helper <- function(disease, varnames = "data_element_shortname", copy_to_clipboard = TRUE) {
  # get msf disease specific data dictionary
  dat_dict <- msf_dict(disease = disease, tibble = FALSE, compact = TRUE)
  msg <- "## Add the appropriate column names after the equals signs\n\n"
  msg <- paste0(msg, "linelist_cleaned <- rename(linelist_cleaned,\n")
  the_renames <- sprintf("  %s =   , # %s",
                         format(dat_dict[[varnames]]),
                         dat_dict[["data_element_valuetype"]])
  the_renames[length(the_renames)] <- gsub(",", " ", the_renames[length(the_renames)])
  msg <- paste0(msg, paste(the_renames, collapse = "\n"), "\n)\n")
  if (copy_to_clipboard) {
    x <- try(clipr::write_clip(msg), silent = TRUE)
    if (inherits(x, "try-error")) {
      if (interactive()) cat(msg)
      return(invisible())
    }
    message("rename template copied to clipboard. Paste the contents to your RMarkdown file and enter in the column names from your data set.")
  } else {
    cat(msg)
  }
}


# function to generate fake dataset based on data dictionary
#' @export
#' @rdname msf_dict
gen_data <- function(dictionary, varnames = "data_element_shortname", numcases = 300) {

  # Three datasets:
  # 1) dat_dict = msf data dicationary generated by (msf_dict)
  # 2) dat_output = formatting of data dictionary to make use for sampling
  # 3) dis_output = dictionary dataset generated from sampling (exported)

  # define which ones are outbreaks and which ones are survey datasets
  SURVEYS <- c("Mortality", "Nutrition", "Vaccination")
  OUTBREAKS <- c("Cholera", "Measles", "Meningitis", "AJS")

  # get msf dictionary specific data dictionary
  if (dictionary %in% SURVEYS) {
    dat_dict <- msf_dict_survey(disease = dictionary, tibble = FALSE, compact = TRUE)
  } else if (dictionary %in% OUTBREAKS) {
    dat_dict <- msf_dict(disease = dictionary, tibble = FALSE, compact = TRUE)
  } else {
    stop("'dictionary' must be one of: 'Cholera', 'Measles', 'Meningitis', 'AJS', 'Mortality', 'Nutrition', 'Vaccination'")
  }


  # # drop extra columns (keep varnames and code options)
  # varcol  <- which(names(dat_dict) == varnames)
  # codecol <- grep("Code", names(dat_dict))
  # dat_output <- dat_dict[, c(varcol, codecol), drop = FALSE]

  
  # # use the var names as rows
  # row.names(dat_output) <- dat_output[[varnames]]
  # # remove the var names column
  # dat_output <- dat_output[-1]
  # # flip the dataset
  # dat_output <- data.frame(t(dat_output))
  # # remove rownames
  # row.names(dat_output) <- NULL

  # # define variables that do not have any contents in the data dictionary

  # # create a NEW empty dataframe with the names from the data dictionary
  # dis_output <- data.frame(matrix(ncol = ncol(dat_output), nrow = numcases) )
  # colnames(dis_output) <- colnames(dat_output)

  # # take samples for vars with defined options (non empties)
  # categories <- lapply(dat_output, function(i) i[!is.na(i)])
  # categories <- categories[lengths(categories) > 0]
  # for (i in names(categories)) {
  #   dis_output[[i]] <- sample(categories[[i]], numcases, replace = TRUE)
  # }

  dis_output <- template_data_frame_categories(dat_dict, numcases, varnames, dictionary %in% SURVEYS)

  # Use data dictionary to define which vars are multiple choice
  # ZNK 2019-05-01 ----
  # These type of columns are currently only present in the survey data sets
  # multivars <- dat_dict[dat_dict$data_element_valuetype == "MULTI", varnames]

  # if (length(multivars) > 0) {
  #   sample_multivars <- lapply(multivars, sample_cats)
  #   sample_multivars <- do.call(cbind, sample_multivars)

  #   dis_output[, multivars] <- NULL
  #   dis_output <- cbind(dis_output, sample_multivars)
  # }


  # Use data dictionary to define which vars are dates
  datevars <- dat_dict[[varnames]][dat_dict$data_element_valuetype == "DATE"]

  # sample between two dates
  posidates <- seq(as.Date("2018-01-01"), as.Date("2018-04-30"), by = "day")

  # fill the date columns with dates
  for (i in datevars) {
    dis_output[[i]] <- sample(posidates, numcases, replace = TRUE)
  }

  if (dictionary %in% OUTBREAKS) {
    # Fix DATES
    # exit dates before date of entry
    # just add 20 to admission.... (was easiest...)
    dis_output <- enforce_timing(dis_output, 
                                 first  = "date_of_consultation_admission", 
                                 second = "date_of_exit", 
                                 20)

    dis_output <- enforce_timing(dis_output, 
                                 first  = "date_of_admission", 
                                 second = "date_of_exit", 
                                 20)
    # lab sample dates before admission
    # add 2 to admission....
    dis_output <- enforce_timing(dis_output,
                                 first  = "date_of_consultation_admission",
                                 second = "date_lab_sample_taken",
                                 2)
    # vaccination dates after admission
    # minus 20 to admission...
    dis_output <- enforce_timing(dis_output,
                                 first  = "date_of_consultation_admission",
                                 second = "date_of_last_vaccination",
                                 20)

#     dis_output$date_of_last_vaccination[dis_output$date_of_exit >
#                                           dis_output$date_of_consultation_admission] <-
#       dis_output$date_of_consultation_admission[dis_output$date_of_exit >
#                                                   dis_output$date_of_consultation_admission] - 20
    # symptom onset after admission
    # minus 20 to admission...
    dis_output <- enforce_timing(dis_output,
                                 first  = "date_of_consultation_admission",
                                 second = "date_of_onset",
                                 20)
    # dis_output$date_of_onset[dis_output$date_of_onset >
    #                            dis_output$date_of_consultation_admission] <-
    #   dis_output$date_of_consultation_admission[dis_output$date_of_onset >
    #                                               dis_output$date_of_consultation_admission] - 20


    # Patient identifiers
    dis_output$case_number <- sprintf("A%d", seq(numcases))

    # treatment site facility
    dis_output$treatment_facility_site <- sample(1:50,
                                                 numcases, replace = TRUE)

    # patient origin (categorical from a dropdown)
    dis_output$patient_origin <- sample(c("Village A", "Village B",
                                          "Village C", "Village D"),
                                        numcases, replace = TRUE)

    # treatment location (categorical from a dropdown)
    dis_output$treatment_location <- sample(c("Ward A", "Ward B",
                                              "Ward C", "Ward D"),
                                            numcases, replace = TRUE)

    # patient origin free text
    dis_output$patient_origin_free_text <- sample(c("Messy location A", "Messy location B",
                                                    "Messy location C", "Messy location D"),
                                                  numcases, replace = TRUE)
  }

  # sample age_month and age_days if appropriate
  age_year_var <- grep("age.*year", names(dis_output), value = TRUE)[1]
  age_month_var <- grep("age.*month", names(dis_output), value = TRUE)[1]
  age_day_var <- grep("age.*day", names(dis_output), value = TRUE)[1]

  # set_age_na controlls if age_year_var should be set to NA if age_month_var is sampled
  # same is done for age_month_var and age_day_var
  set_age_na <- TRUE
  if (dictionary == "Mortality") {
    set_age_na <- FALSE
  }

  if (!is.na(age_year_var)) {
    # sample 0:120
    dis_output[, age_year_var] <- sample(0:120L, numcases, replace = TRUE)
    U2_YEARS <- which(dis_output[, age_year_var] <= 2)
    if (set_age_na) {
      dis_output[U2_YEARS, age_year_var] <- NA_integer_
    }

    U2_MONTHS <- integer(0)
    if (!is.na(age_month_var)) {
      # age_month
      if (length(U2_YEARS) > 0) {
        dis_output[U2_YEARS, age_month_var] <- sample(0:24L,
                                                      length(U2_YEARS),
                                                      replace = TRUE)
        U2_MONTHS <- which(dis_output[, age_month_var] <= 2)
        if (set_age_na) {
          dis_output[U2_MONTHS, age_month_var] <- NA_integer_
        }
      }

      if (!is.na(age_day_var)) {
        # age_days
        if (length(U2_MONTHS) > 0) {
          dis_output[U2_MONTHS, age_day_var] <- sample(0:60L,
                                                       length(U2_MONTHS),
                                                       replace = TRUE)
        }
      }
    }
  }



  if (dictionary == "Cholera" | dictionary == "Measles" |
      dictionary == "AJS") {
    # fix pregnancy stuff
    dis_output$pregnant[dis_output$sex != "F"] <- "NA"
    PREGNANT_FEMALE <- dis_output$sex != "F" | dis_output$pregnant != "Y"

    dis_output$foetus_alive_at_admission[PREGNANT_FEMALE]  <- NA
    dis_output$trimester[PREGNANT_FEMALE]                  <- NA
    dis_output$delivery_event[PREGNANT_FEMALE]             <- "NA"
    dis_output$pregnancy_outcome_at_exit[PREGNANT_FEMALE]  <- NA
    dis_output$pregnancy_outcome_at_exit[dis_output$delivery_event != "1"] <- NA
  }


  if (dictionary == "Cholera") {
    dis_output$ors_consumed_litres <- sample(1:10, numcases, replace = TRUE)
    dis_output$iv_fluids_received_litres <- sample(1:10, numcases, replace = TRUE)
  }

  if (dictionary == "Measles") {
    dis_output$baby_born_with_complications[PREGNANT_FEMALE &
                                             dis_output$delivery_event != "1"] <- NA

    # fix vaccine stuff among non vaccinated
    NOTVACC <- which(!dis_output$previously_vaccinated %in% c("C", "V"))

    dis_output$previous_vaccine_doses_received[NOTVACC] <- NA
    dis_output$date_of_last_vaccination[NOTVACC] <- NA

  }

  if (dictionary == "Meningitis") {
    # T1 lab sample dates before admission
    # add 2 to admission....

    dis_output <- enforce_timing(dis_output,
                                 first  = "date_of_consultation_admission",
                                 second = "date_ti_sample_sent",
                                 2)
    # dis_output$date_ti_sample_sent[dis_output$date_ti_sample_sent <=
    #                                    dis_output$date_of_consultation_admission] <-
    #   dis_output$date_of_consultation_admission[dis_output$date_ti_sample_sent <=
    #                                               dis_output$date_of_consultation_admission] + 2

    # fix pregnancy delivery
    dis_output$delivery_event[dis_output$sex != "F"] <- "NA"

    # fix vaccine stuff among not vaccinated
    NOTVACC <- which(!dis_output$vaccinated_meningitis_routine %in% c("C", "V") &
                     !dis_output$vaccinated_meningitis_mvc %in% c("C", "V"))

    dis_output$name_meningitis_vaccine[NOTVACC] <- NA
    dis_output$date_of_last_vaccination[NOTVACC] <- NA
  }

  if (dictionary == "Mortality") {
    # q65_iq4 GPS number (from Osmand) - use as a standin for fact_0_id as household num for now
    dis_output$q65_iq4 <- sample(1:as.integer(numcases/5), numcases, replace = TRUE)
    dis_output$fact_0_id <- dis_output$q65_iq4

    # q53_cq4a ("Why is no occupant agreeing to participate?") shoud be NA if
    # Head of Household answers the questions (q49_cq3)
    dis_output$q53_cq4a[dis_output$q49_cq3 == "Yes"] <- factor(NA, levels(dis_output$q53_cq4a))
    # assume person is not born during study when age > 1
    dis_output$q87_q32_born[dis_output$q155_q5_age_year > 1] <- factor("No", levels(dis_output$q87_q32_born))
    dis_output$q88_q33_born_date[dis_output$q155_q5_age_year > 1] <- NA
    # pregnancy set to NA for males
    dis_output$q152_q7_pregnant[dis_output$q4_q6_sex == "Male"] <- NA

    # resample death yes/no to have lower death rates
    dis_output$q136_q34_died <- sample(c("Yes", "No"), nrow(dis_output), prob = c(0.05, 0.95), replace = TRUE)
    # set Columns that are relate to "death" as NA if "q136_q34_died" is "No"
    died <- dis_output$q136_q34_died == "No"
    dis_output[died, c("q137_q35_died_date", "q138_q36_died_cause",
                       "q141_q37_died_violence", "q143_q41_died_place",
                       "q145_q43_died_country")] <- NA
    # pregnancy related cause of death n.a. for too old/young and for males
    no_pregnancy <- dis_output$q138_q36_died_cause == "Pregnancy-related" &
                      (dis_output$q4_q6_sex == "Male"    | 
                       dis_output$q155_q5_age_year >= 50 |
                       dis_output$q155_q5_age_year < 12
                      )

    no_pregnancy[is.na(no_pregnancy)] <- FALSE # replace NAs
    dis_output[no_pregnancy, "q138_q36_died_cause"] <- "Unknown"

    # fix arrival/leave dates
    dis_output$q41_q25_hh_arrive_date <-
      pmin(dis_output$q41_q25_hh_arrive_date, 
           dis_output$q45_q29_hh_leave_date, 
           dis_output$q88_q33_born_date, na.rm = TRUE)

    # leave date
    chn_date <- dis_output$q45_q29_hh_leave_date <= dis_output$q41_q25_hh_arrive_date
    chn_date2 <- dis_output$q45_q29_hh_leave_date < dis_output$q88_q33_born_date
    chn_date[is.na(chn_date)] <- FALSE
    chn_date2[is.na(chn_date2)] <- FALSE

    dis_output$q45_q29_hh_leave_date[chn_date] <- dis_output$q41_q25_hh_arrive_date[chn_date] + sample(5:30, sum(chn_date), replace = TRUE)
    dis_output$q45_q29_hh_leave_date[chn_date2] <- dis_output$q88_q33_born_date[chn_date2] + sample(5:30, sum(chn_date2), replace = TRUE)

    # died date
    chn_date <- dis_output$q137_q35_died_date <= dis_output$q41_q25_hh_arrive_date
    chn_date2 <- dis_output$q137_q35_died_date < dis_output$q88_q33_born_date
    chn_date[is.na(chn_date)] <- FALSE
    chn_date2[is.na(chn_date2)] <- FALSE
    dis_output$q137_q35_died_date[chn_date] <- dis_output$q41_q25_hh_arrive_date[chn_date] + sample(5:30, sum(chn_date), replace = TRUE)
    dis_output$q137_q35_died_date[chn_date2] <- dis_output$q88_q33_born_date[chn_date2] + sample(5:30, sum(chn_date2), replace = TRUE)

    dis_output$q45_q29_hh_leave_date[!is.na(dis_output$q137_q35_died_date)] <- NA

    # more plausibility checks of generated data might be implemented in the future
  }

  if (dictionary == "Nutrition") {

    # sample villages
    dis_output$village <- sample(c("Village A", "Village B",
                                   "Village C", "Village D"),
                                 numcases, replace = TRUE)

    # make two health districts
    dis_output$health_district <- ifelse(dis_output$village == "Village A" |
                                           dis_output$village == "Village B",
                                         "District A", "District B")

    # cluster ID (based on village)
    dis_output$cluster_number <- as.numeric(factor(dis_output$village))

    # household number just each its own
    dis_output$household_id <- 1:numcases

    # age in months (1 to 60 - i.e. under 5 years)
    dis_output$age_month <- sample(1:60L, numcases, replace = TRUE)

    # height in cm
    dis_output$height <- round(
      runif(numcases, 40, 120),
      digits = 1)

    # weight in kg
    dis_output$weight <- round(
      runif(numcases, 2, 30),
      digits = 1)

    # MUAC in mm
    dis_output$muac_mm_left_arm <- sample(80:190, numcases, replace = TRUE)

  }
  if (dictionary == "Vaccination") {

    # cluster number (1 to a 30th of total cases)
    dis_output$q77_what_is_the_cluster_number <- sample(1:as.integer(numcases/30), numcases, replace = TRUE)

    # household ID (numbering starts again for each cluster)
    for (i in unique(dis_output$q77_what_is_the_cluster_number)) {

      nums <- nrow(dis_output[dis_output$q77_what_is_the_cluster_number == i,])

      dis_output[dis_output$q77_what_is_the_cluster_number == i, "q14_hh_no"] <- sample(1:(as.integer(nums/5) + 1), nums, replace = TRUE)
    }
    dis_output$fact_0_id <- dis_output$q14_hh_no


    # age in yr (0 to 14) - assuming doing vaccination coverage among those aged less than 15 yrs
    dis_output$q10_age_yr <- sample(0:14L, numcases, replace = TRUE)

    # age in mth (0 to 11)
    dis_output$q55_age_mth[dis_output$q10_age_yr < 1] <- sample(0:11L,
                                                                nrow(dis_output[dis_output$q10_age_yr < 1,]),
                                                                replace = TRUE)

  }


  # return dataset as a tibble
  dplyr::as_tibble(dis_output)

}





