
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sitrep

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/R4EPI/sitrep.svg?branch=master)](https://travis-ci.org/R4EPI/sitrep)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/R4EPI/sitrep?branch=master&svg=true)](https://ci.appveyor.com/project/R4EPI/sitrep)
[![Codecov test
coverage](https://codecov.io/gh/R4EPI/sitrep/branch/master/graph/badge.svg)](https://codecov.io/gh/R4EPI/sitrep?branch=master)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/sitrep)](https://CRAN.R-project.org/package=sitrep)
<!-- badges: end -->

The goal of {sitrep} is provide report templates for common
epidemiological surveys and outbreak reports. The package further
contains helper function that standardize certain analyses.

**Detailed information about the project and the templates can be found
at <https://r4epis.netlify.com>.** An online manual for the functions in
{sitrep} can be found at <https://r4epi.github.io/sitrep>.

## Installation

You can install the released version of sitrep from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("sitrep")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("R4EPI/sitrep")
```

## Available templates

Sitrep has [four outbreak
templates](https://r4epis.netlify.com/outbreaks/) and [three survey
templates](https://r4epis.netlify.com/surveys/) available. These
templates will generate the following:

1.  A word document with the situation report
2.  A plain text markdown document (for conversion to other formats such
    as HTML or PDF)
3.  A directory with all of the figures produced

You can generate an example template by using the
`check_sitrep_templates()` function:

``` r
library("sitrep")
output_dir <- file.path(tempdir(), "sitrep_example")
dir.create(output_dir)

# view the available templates, categorized by type
available_sitrep_templates(categorise = TRUE)
#> $outbreak
#> [1] "ajs_outbreak"        "cholera_outbreak"    "measles_outbreak"   
#> [4] "meningitis_outbreak"
#> 
#> $survey
#> [1] "mortality"   "nutrition"   "vaccination"

# generate the measles outbreak template in the output directory
check_sitrep_templates("measles_outbreak", path = output_dir)
#> [1] "/tmp/RtmpkK0upp/sitrep_example"

# view the contents
list.files(output_dir, recursive = TRUE)
#>  [1] "measles_outbreak_files/figure-docx/age_pyramid-1.png"              
#>  [2] "measles_outbreak_files/figure-docx/bar_attack_rate_by_region-1.png"
#>  [3] "measles_outbreak_files/figure-docx/biweekly_epicurve-1.png"        
#>  [4] "measles_outbreak_files/figure-docx/choropleth_maps-1.png"          
#>  [5] "measles_outbreak_files/figure-docx/epicurve_ar_cfr-1.png"          
#>  [6] "measles_outbreak_files/figure-docx/epicurve-1.png"                 
#>  [7] "measles_outbreak_files/figure-docx/incidence_by_case_def-1.png"    
#>  [8] "measles_outbreak_files/figure-docx/incidence_by_gender-1.png"      
#>  [9] "measles_outbreak_files/figure-docx/incidence_by_sex_facility-1.png"
#> [10] "measles_outbreak_files/figure-docx/incidence_by_vax_status-1.png"  
#> [11] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-1.png"     
#> [12] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-10.png"    
#> [13] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-11.png"    
#> [14] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-12.png"    
#> [15] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-13.png"    
#> [16] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-14.png"    
#> [17] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-15.png"    
#> [18] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-16.png"    
#> [19] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-17.png"    
#> [20] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-18.png"    
#> [21] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-19.png"    
#> [22] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-2.png"     
#> [23] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-20.png"    
#> [24] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-3.png"     
#> [25] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-4.png"     
#> [26] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-5.png"     
#> [27] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-6.png"     
#> [28] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-7.png"     
#> [29] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-8.png"     
#> [30] "measles_outbreak_files/figure-docx/map_for_loop_epiweek-9.png"     
#> [31] "measles_outbreak.md"                                               
#> [32] "measles_outbreak.Rmd"
```

Please note that the ‘sitrep’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
