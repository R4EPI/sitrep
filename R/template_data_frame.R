template_data_frame_categories <- function(dat_dict, numcases, varnames, survey = FALSE) {

  dat_output <- dat_dict[, c(varnames, "options"), drop = FALSE]

  # create a NEW empty dataframe with the names from the data dictionary
  dis_output <- data.frame(matrix(ncol = nrow(dat_output), nrow = numcases))

  colnames(dis_output) <- dat_dict[[varnames]]


  if (utils::packageVersion("tidyr") > "0.8.99") {
    categories <- tidyr::unnest(dat_dict, cols = "options")
  } else {
    categories <- tidyr::unnest(dat_dict)
  }
  categories <- dplyr::filter(categories, !is.na(!! quote(option_name)))

  # take samples for vars with defined options (non empties)
  for (i in unique(categories[[varnames]])) {
    vals <- categories[categories[[varnames]] == i, ]
    if (survey) {
      vals <- factor(vals$option_name, vals$option_name[vals$option_order_in_set])
    } else {
      vals <- factor(vals$option_code, vals$option_code[vals$option_order_in_set])
    }
    dis_output[[i]] <- sample(vals, numcases, replace = TRUE)
  }

  multivars <- dat_dict[[varnames]][dat_dict$data_element_valuetype == "MULTI"]

  if (length(multivars) > 0) {
    sample_multivars <- lapply(multivars, sample_cats, numcases = numcases, 
                               df = categories, varnames = varnames)

    dis_output[, multivars] <- NULL
    dis_output <- dplyr::bind_cols(dis_output, sample_multivars)
  }

  dis_output
}


# Enforces timing between two columns in a data frame.
#
# The data in the first column must come before the second column. If the timing
# isn't correct, then force the timing to be correct by making the second column
# bigger than the first by `add`.
enforce_timing <- function(x, first, second, add = 2) {

  mistakes              <- x[[second]] <= x[[first]]
  x[[second]][mistakes] <- x[[first]][mistakes] + add
  x 

}

# sample of a single value and NA
sample_single <- function(x, size, prob = 0.1) {
  sample(c(x, NA), size = size, prob = c(prob, 1 - prob), replace = TRUE)
}


# random data for one single "MULTI" variable (split into multiple columns)
sample_cats <- function(category, numcases, df, varnames) {

  dat <- df[df[[varnames]] == category, , drop = FALSE]

  lvls <- dat$option_name
  # define suffixes for column names, e.g. 000, 001, 002, ...
  suffixes <- formatC((seq_along(lvls)) - 1, width = 3, format = "d", flag = "0")

  # create columns with randomized lvls with randomized probability
  extra_cols <- vapply(lvls, sample_single, FUN.VALUE = character(numcases), 
                       size = numcases, prob = sample(5:15, 1) / 100)
  colnames(extra_cols) <- paste0(category, "_", suffixes)
  as.data.frame(extra_cols, stringsAsFactors = FALSE)
}
