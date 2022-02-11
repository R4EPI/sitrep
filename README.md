sitrep
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/sitrep)](https://CRAN.R-project.org/package=sitrep)
[![Codecov test
coverage](https://codecov.io/gh/R4EPI/sitrep/branch/master/graph/badge.svg)](https://codecov.io/gh/R4EPI/sitrep?branch=master)
[![R build
status](https://github.com/R4EPI/sitrep/workflows/R-CMD-check/badge.svg)](https://github.com/R4EPI/sitrep/actions)
<!-- badges: end -->

The goal of {sitrep} is provide report templates for common
epidemiological surveys and outbreak reports. The package further
contains helper function that standardize certain analyses.

While templates are primarily for MSF analyses - they have been setup to
be as generic as possible for use by the general applied epidemiology
community.

**Detailed information about the project and the templates can be found
at <https://r4epis.netlify.com>.**  
An online manual for the functions in {sitrep} can be found at
<https://r4epi.github.io/sitrep>.

{sitrep} includes a number of other *R* packages which facilitate
specific analysis:  
[{epitabulate}](https://r4epi.github.io/epitabulate): Tables for
epidemiological analysis  
[{epidict}](https://r4epi.github.io/epidict): Epidemiology data
dictionaries and random data generators  
[{epikit}](https://r4epi.github.io/epikit): Miscellaneous helper tools
for epidemiologists  
[{apyramid}](https://r4epi.github.io/apyramid): Age pyramid construction
and plotting

## Installation

The **{sitrep}** package, developed by MSF and partners, is stored in a
[GitHub repository](https://github.com/R4EPI/sitrep). Therefore, the
procedure to install these packages have one extra step required.

To install **sitrep** from GitHub you must first install the **drat**
package.

``` r
install.packages("drat", repos = "https://cran.rstudio.com")
```

Once **drat** is installed, use its `addRepo()` function to add the
“R4EPI” project to the list of valid repositories. This command below
will install the [latest release of the **{sitrep}**
package](https://github.com/R4EPI/sitrep/releases) and also install any
packages necessary for use of the templates that are not currently
installed on your machine.

``` r
drat::addRepo("R4EPI")
install.packages("sitrep")
```

If you are getting errors, check the [frequently asked
questions](https://r4epis.netlify.com/faq/).

## Available templates

Sitrep has [four outbreak
templates](https://r4epis.netlify.com/outbreaks/) and [three survey
templates](https://r4epis.netlify.com/surveys/) available. These
templates will generate the following:

1.  A word document with the situation report
2.  A plain text markdown document (for conversion to other formats such
    as HTML or PDF)
3.  A directory with all of the figures produced

You can access the list of templates in *R Studio* by clicking (see
example below): file &gt; New file &gt; R Markdown… &gt; From Template

![Example of how to open and save the cholera
template](https://github.com/R4EPI/R4EPIs-website/raw/master/content/welcome/images/opening_template.gif)

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
#> [1] "C:\\Users\\Spina\\AppData\\Local\\Temp\\RtmpgTPWRw/sitrep_example"

# view the contents
list.files(output_dir, recursive = TRUE)
#> [1] "measles_outbreak.Rmd"
```

Please note that the ‘sitrep’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
