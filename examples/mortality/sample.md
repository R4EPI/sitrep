Mortality survey
================

``` r
# study_data_raw <- readxl::read_excel("...")
study_data_raw <- outbreaks::fluH7N9_china_2013

study_data_cleaned <- study_data_raw %>% 
  # some cleaning
  mutate(age_in_years = as.integer(age),
         age_group = cut(age_in_years, breaks = seq(0, 90, 5), right = FALSE),
         sex = gender,
         symp_Tuberculosis = runif(n()) < 0.5,
         symp_Cancer = runif(n()) < 0.01,
         symp_Mumps = runif(n()) < 0.1,
         died = outcome == "Death") %>% 
  filter(!is.na(sex))
```

# Introduction

# Study Design

# Sample Size and Sampling methods

## Sample Size

``` r
size <- sample_size(population_size = 10000,
                    precision = 0.005, 
                    design_effect = 1.5, 
                    expected_prevalence = 0.005)
size
```

    ## [1] 1065

``` r
sample_size_households(size, 
                       avg_hh = 5, 
                       prop_under_5 = 0.15, 
                       frac_6_59 = 0.9, 
                       non_response_rate = 0.03)
```

    ## [1] 1627

## Sampling

### Random sampling

``` r
# Insert some code to generate a random number sequence
```

### Cluster sampling

``` r
# Insert some example code to do cluster sampling
```

# Study Findings

``` r
# we first need to create a survery design that reflects the study
suvery_design <- svydesign(
  ids = ~1, # no cluster ids
  weights = NULL, #no sampling weights in this example
  data = study_data_cleaned
)
```

## Demographics

``` r
plot_age_pyramid(study_data_cleaned)
```

![](sample_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Morbitity

``` r
# symptoms start with sym_

cols <- colnames(study_data_cleaned)
symptom_columns <- cols[grepl(pattern = "^sym", x = cols)]

survey_ciprop_foreach_binary_variable(design = suvery_design, 
                                      variables = symptom_columns) %>% 
  knitr::kable()
```

| variable           |  n |      mean | CI\_lower | CI\_upper |
| :----------------- | -: | --------: | --------: | --------: |
| symp\_Tuberculosis | 70 | 0.5223881 | 0.4368997 | 0.6065847 |
| symp\_Cancer       |  1 | 0.0074627 | 0.0010238 | 0.0522765 |
| symp\_Mumps        | 12 | 0.0895522 | 0.0511848 | 0.1520699 |

## Mortality

``` r
suvery_ciprob(suvery_design, "died") %>% 
  knitr::kable()
```

|  n |      mean | CI\_lower | CI\_upper |
| -: | --------: | --------: | --------: |
| 31 | 0.4025974 |  0.298175 | 0.5166676 |

``` r
suvery_ciprob_stratified(suvery_design, "died", "age_group") %>% 
  kable()
```

| by       |      mean |   CI\_lower | CI\_upper |
| :------- | --------: | ----------: | --------: |
| \[0,5)   | 0.0000000 |   0.0000000 | 0.0000000 |
| \[5,10)  | 0.3333333 | \-0.0452773 | 0.7119439 |
| \[10,15) | 0.2222222 | \-0.0504086 | 0.4948530 |
| \[15,20) | 0.0000000 |   0.0000000 | 0.0000000 |
| \[20,25) | 0.3333333 | \-0.0452773 | 0.7119439 |
| \[25,30) | 0.5000000 |   0.1522239 | 0.8477761 |
| \[30,35) | 0.6000000 |   0.1689821 | 1.0310179 |
| \[35,40) | 0.4545455 |   0.1591891 | 0.7499018 |
| \[40,45) | 0.2000000 | \-0.1519246 | 0.5519246 |
| \[45,50) | 0.6250000 |   0.2882673 | 0.9617327 |
| \[50,55) | 0.3333333 | \-0.0452773 | 0.7119439 |
| \[55,60) | 1.0000000 |   1.0000000 | 1.0000000 |
| \[60,65) | 0.5000000 | \-0.1955521 | 1.1955521 |

# Conclusions

# Recommendations

# References
