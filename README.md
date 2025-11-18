
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Sitrep <img src="man/figures/logo.png" align="right" width="200" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/sitrep)](https://CRAN.R-project.org/package=sitrep)
[![R-CMD-check](https://github.com/R4EPI/sitrep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R4EPI/sitrep/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/R4EPI/sitrep/graph/badge.svg)](https://app.codecov.io/gh/R4EPI/sitrep)
<!-- badges: end -->

The goal of {sitrep} is provide report templates for common
epidemiological surveys and outbreak reports. The package further
contains helper function that standardize certain analyses.

While templates are primarily for Medecins Sans Frontieres analyses -
they have been setup to be as generic as possible for use by the general
applied epidemiology community.

**Detailed information about the project, the templates and functions in
{sitrep} can be found at <https://r4epi.github.io/sitrep/>.**

{sitrep} includes a number of other *R* packages which facilitate
specific analysis:  
[{epitabulate}](https://r4epi.github.io/epitabulate/): Tables for
epidemiological analysis  
[{epidict}](https://r4epi.github.io/epidict/): Epidemiology data
dictionaries and random data generators  
[{epikit}](https://r4epi.github.io/epikit/): Miscellaneous helper tools
for epidemiologists  
[{apyramid}](https://r4epi.github.io/apyramid/): Age pyramid
construction and plotting

## Installation

You can install {sitrep} from CRAN:

``` r
install.packages("sitrep")
```

<details>

<!--
NOTE: everything inside the details tag will be collapsed and effectively
hidden from the user
-->

<summary style="text-decoration: underline">

Click here for alternative installation options
</summary>

You can also install the in-development version from GitHub using the
{remotes} package (but there’s no guarantee that it will be stable):

``` r
# install.packages("remotes")
remotes::install_github("R4EPI/sitrep") 
```

</details>

     
    ## Available templates

    Sitrep has outbreak templates and survey templates available. These templates 
    will generate the following:

    1. A word document with the situation report
    2. A plain text markdown document (for conversion to other formats such as HTML or PDF)
    3. A directory with all of the figures produced

    You can access the list of templates in *R Studio* by clicking (see example below): 
    file > New file > R Markdown... > From Template

    You can generate an example template by using the `check_sitrep_templates()`
    function:


    ``` r
    library("sitrep")
    output_dir <- file.path(tempdir(), "sitrep_example")
    dir.create(output_dir)

    # view the available templates, categorized by type
    available_sitrep_templates(categorise = TRUE)
    #> $outbreak
    #>  [1] "ajs_intersectional_outbreak"        "ajs_outbreak"                      
    #>  [3] "ajs_outbreak_recode"                "cholera_intersectional_outbreak"   
    #>  [5] "cholera_outbreak"                   "cholera_outbreak_recode"           
    #>  [7] "diphtheria_intersectional_outbreak" "intersectional_outbreak_recode"    
    #>  [9] "measles_intersectional_outbreak"    "measles_outbreak"                  
    #> [11] "measles_outbreak_recode"            "meningitis_intersectional_outbreak"
    #> [13] "meningitis_outbreak"                "meningitis_outbreak_recode"        
    #> 
    #> $survey
    #> [1] "ebs"                      "mortality"               
    #> [3] "mortality_recode"         "nutrition"               
    #> [5] "nutrition_recode"         "vaccination_long"        
    #> [7] "vaccination_long_recode"  "vaccination_short"       
    #> [9] "vaccination_short_recode"

    # generate the measles outbreak template in the output directory
    check_sitrep_templates("measles_outbreak", path = output_dir)
    #> [1] "C:\\Users\\spina\\AppData\\Local\\Temp\\RtmpMfBkNj/sitrep_example"

    # view the contents
    list.files(output_dir, recursive = TRUE)
    #> [1] "measles_outbreak.Rmd"
