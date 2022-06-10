---
title: "AJS outbreak report"
output: 
  word_document:
    keep_md: true
---

# Introduction to this template

This is a template which can be used to create an automated outbreak situation
report for acute jaundice syndrome (AJS). 

- It is organised by time, place and person. 
For a more detailed explanation of this template, please visit https://r4epis.netlify.com/outbreaks
- Feedback and suggestions are welcome at the [GitHub issues page](https://github.com/R4EPI/sitrep/issues)
- Text within <! > will not show in your final document. These comments are used
to explain the template. You can delete them if you want.

<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This comment will not show up when you knit the document.

A comment with a title with slashes indicates a name of a code chunk.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->


## Installing and loading required packages 
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// setup \\\
--------------------------------------------------------------------------------

Several packages are required for different aspects of  analysis with *R*. 
You will need to install these before starting. 

These packages can be quite large and may take a while to download in the
field. If you have access to a USB key with these packages, it makes sense to
copy and paste the packages into your computer's R package library 
(run the command .libPaths() to see the folder path). 

For help installing packages, please visit https://r4epis.netlify.com/welcome
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->








<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// define_current_week \\\
--------------------------------------------------------------------------------

You need to set the week you want to report on. Generally, this is the previous
week. Put it below.

The standard is
Monday.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->






<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// set_plot_defaults \\\
--------------------------------------------------------------------------------

This code set the defaults for plots, such as the base text size,
the text angle, legend title, gridline color (grey), and x and y labels
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->









<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// read_fake_data \\\
--------------------------------------------------------------------------------

To play with this template, you can create fake data. 
Comment out the code in this chunk when you are using real data.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->









<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If using real data, it should be in a "linelist" format: one row per case

There are two options:

1) Your data are from DHIS2: use the "read_DHIS_data" code below  
2) Your data are NOT FROM DHIS2 and are in Excel, CSV, or Stata format: 
use the "read_nonDHIS_data" code below
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->









<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// read_DHIS_data \\\
--------------------------------------------------------------------------------

This section is for data from DHIS2.
It uses the standardized MSF data dictionary.
If you did not use the standardized data dictionary, go to read_nonDHIS_data.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->





<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// prep_DHIS_data \\\
--------------------------------------------------------------------------------

This section is for data from DHIS2 that you loaded in read_DHIS_data.

If you did not use DHIS2 data, go to read_nonDHIS_data.

This step shows you the AJS data dictionary. The data dictionary has variable 
names in the data_element_shortname column. Possible values for each variable are
specified in code and name columns. Code has the shortened value and Name has
the full-text value.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->





<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// read_nonDHIS_data \\\
--------------------------------------------------------------------------------

This section is to import data not from DHIS2.
If you have already imported data from DHIS2, go to read_population_data.

Uncomment and use the code appropriate to your situation. Then update the
filepath and name of your file (and sheet or range if applicable)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->





<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// prep_nonDHIS_data \\\
--------------------------------------------------------------------------------

This section is for data not from DHIS2. 
If you have already loaded data from DHIS2, go to read_population_data.

It is more difficult to prepare the nonDHIS data. You can do it! It will just
take a little more work.

Checklist to update this script to match your data:
[ ] Comment out all lines in read_DHIS_data and prep_DHIS_data
[ ] Recode your variable names to match the dictionary
[ ] Recode variable contents to match the dictionary


This step shows you the data dictionary. The data dictionary has variable names
in the data_element_shortname column. Possible values for each variable are
specified in code and name columns. Code has the shortened value and Name has
the full-text value.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->






<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// read_population_data \\\
--------------------------------------------------------------------------------

This template uses population data to calculate things like incidence.

There are three options:
- You can read in a spreadsheet with age group and region data.
- You can put in the specific populations into the gen_population() function. 
- If you have the total or regional populations, you can estimate the age group
from proportions.

Comment out the options you are not using.

Age group proportions are from the OCBA population denominators tool v1. The
proportions below are for sub-Saharan Africa in 2019. They are only an estimate!
If you have more specific proportions, you can use them below.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->






<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// lab_data \\\
--------------------------------------------------------------------------------

If you have lab data, you can read it in here.
You will need to make sure that your identifier (e.g. case_number) matches the
linelist.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->




<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// browse_data \\\
--------------------------------------------------------------------------------

You'll want to look at your data. Here are a few ways you can explore.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->





<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// remove_unused_data \\\
--------------------------------------------------------------------------------

Your data might have empty rows or columns you want to remove.
You can also use this section to create temporary datasets so you can review
specific variables or rows.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

```
## filter: no rows removed
```



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This part of the script will create and clean variables in your data.

All your cleaning and variable creation should happen in these chunks.
That way, in case something goes wrong, you can push the small arrow at the top
of the chunk to re-run all the code chunks up to the current one.

The chunks are:
- standardise_dates -- will set up and clean dates.
- create_age_group  -- creates the age group variables from age
- create_vars       -- creates variables based on other variables
- factor_vars       -- helps clean factor variables
- vector_vars       -- creates groups of variables for easy use


You must adapt this section according to your data!
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// standardise_dates \\\
--------------------------------------------------------------------------------

This chunk will help you set up and clean your date variables.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

```
## filter: removed 209 rows (97%), 6 rows remaining
```

```
## filter: no rows removed
```

```
## mutate: no changes
```

```
## mutate: new variable 'epiweek' (double) with 19 unique values and 0% NA
```

```
##         new variable 'epiweek_date' (Date) with 19 unique values and 0% NA
```



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// create_age_group \\\ 
--------------------------------------------------------------------------------

This chunk will help you set up your age group variable.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

```
## mutate: converted 'age_years' from integer to double (16 fewer NA)
```

```
##         converted 'age_months' from integer to double (2 fewer NA)
```

```
## mutate: new variable 'age_group_mon' (factor) with 4 unique values and 96% NA
```

```
## mutate: new variable 'age_group' (factor) with 5 unique values and 0% NA
```



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// create_vars \\\
--------------------------------------------------------------------------------

This chunk will help you construct new variables from other variables. It
includes numeric, factor, and character vectors.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

```
## mutate: new variable 'obs_days' (double) with 78 unique values and 0% NA
```

```
## mutate: new variable 'DIED' (logical) with 2 unique values and 0% NA
```

```
## mutate: new variable 'exit_status2' (character) with 4 unique values and 0% NA
```

```
## mutate: new variable 'case_def' (character) with 3 unique values and 0% NA
```
  


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// factor_vars \\\
--------------------------------------------------------------------------------

This chunk will help you clean factor variables.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

```
## mutate: changed 88 values (29%) of 'sex' (88 new NA)
```

```
## mutate: no changes
```

```
## Warning: Outer names are only allowed for unnamed scalar atomic inputs

## Warning: Outer names are only allowed for unnamed scalar atomic inputs

## Warning: Outer names are only allowed for unnamed scalar atomic inputs

## Warning: Outer names are only allowed for unnamed scalar atomic inputs

## Warning: Outer names are only allowed for unnamed scalar atomic inputs

## Warning: Outer names are only allowed for unnamed scalar atomic inputs

## Warning: Outer names are only allowed for unnamed scalar atomic inputs
```

```
## mutate: no changes
```



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// vector_vars \\\
--------------------------------------------------------------------------------

This chunk creates groups of variables that you might want to use together. That
way, if you want to run the same function over these variables, you can run it
all at once. For example, you may want to look at frequency of all symptoms at
the same time.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->




<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// report_setup \\\
--------------------------------------------------------------------------------

This chunk removes cases after your reporting week and defines the start and end
of the reporting period.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

```
## filter: no rows removed
```



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// save_cleaned_data \\\
--------------------------------------------------------------------------------

You can save your cleaned dataset as an Excel. 
This automatically names your file "linelist_cleaned_DATE", where DATE is the
current date.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->







<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After adjusting and running the above code, you will have a clean dataset.
This marks the start of the ANALYSIS portion of the template.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->



### Person
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This section focuses on who is affected - total, by sex, by age.
There is code to include:
- A bar chart of case numbers of incidence by age group.
- Attack rate (AR)
- Number of deaths (in suspected and confirmed cases)
- Mortality rates
- Case fatality ratio (CFR)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->



From the start of the outbreak up until 2019 W25 there were a 
total of 300 cases. There were
105 (35.0%) females and
107 (35.7%) males. 

The most affected age group was 45+ years. 



#### Demographics 

Cases by age group and definition 
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// describe_by_age_group_and_def \\\
--------------------------------------------------------------------------------

This chunk will create a table of cases by age group and case definition.
You can only use this chunk if you have lab results and have defined a case
definition variable.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

```
## select: dropped one variable (variable)
```

```
## rename: renamed one variable (Age group)
```



|Age group | Confirmed cases (n)|     %| Probable cases (n)|     %| Suspected cases (n)|     %| Total|
|:---------|-------------------:|-----:|------------------:|-----:|-------------------:|-----:|-----:|
|0-2       |                   6|   5.8|                  6|   6.5|                   4|   3.9|    16|
|3-14      |                  19|  18.3|                 19|  20.4|                  25|  24.3|    63|
|15-29     |                  27|  26.0|                 17|  18.3|                  29|  28.2|    73|
|30-44     |                  26|  25.0|                 18|  19.4|                  11|  10.7|    55|
|45+       |                  26|  25.0|                 33|  35.5|                  34|  33.0|    93|
|Total     |                 104| 100.0|                 93| 100.0|                 103| 100.0|   300|


  
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// total_props_agegroup_sex \\\
--------------------------------------------------------------------------------

You can also show cases by proportion of the total population by sex and age.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Cases by age group and sex 

```
## select: dropped one variable (variable)
```

```
## rename: renamed one variable (Age group)
```



|Age group | Male cases (n)|    %| Female cases (n)|    %| Missing cases (n)|    %| Total|
|:---------|--------------:|----:|----------------:|----:|-----------------:|----:|-----:|
|0-2       |              4|  1.3|                8|  2.7|                 4|  1.3|    16|
|3-14      |             22|  7.3|               22|  7.3|                19|  6.3|    63|
|15-29     |             26|  8.7|               26|  8.7|                21|  7.0|    73|
|30-44     |             22|  7.3|               16|  5.3|                17|  5.7|    55|
|45+       |             33| 11.0|               33| 11.0|                27|  9.0|    93|
|Total     |            107| 35.7|              105| 35.0|                88| 29.3|   300|

  
Age pyramid 

There were 88 (29.3%) cases missing information on sex and 0 (0.0%) missing age group.



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// age_pyramid \\\
--------------------------------------------------------------------------------

This chunk creates an age/sex pyramid of your cases.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
![](skeleton_files/figure-docx/age_pyramid-1.png)<!-- -->



Of the patients, 146 (48.7%) were seen as outpatients and 154 (51.3%) were inpatients. Among inpatients, the median number of days admitted was 20, 
with a range between 1 and 112 days.



Cases by symptoms
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// describe_by_symptoms \\\
--------------------------------------------------------------------------------

This chunk gives the counts and proportions for all of the variables in SYMPTOMS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(SYMPTOMS)` instead of `SYMPTOMS` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
## select: dropped one variable (value)
## 
## mutate: changed 13 values (100%) of 'variable' (0 new NA)
```



|variable                  |   n|    %|
|:-------------------------|---:|----:|
|History of fever          | 144| 48.0|
|Fever                     | 142| 47.3|
|Nausea anorexia           | 156| 52.0|
|Vomiting                  | 157| 52.3|
|Epigastric pain heartburn | 163| 54.3|
|Generalized itch          | 140| 46.7|
|Headache                  | 153| 51.0|
|Joint pains               | 145| 48.3|
|Diarrhoea                 | 159| 53.0|
|Bleeding                  | 138| 46.0|
|Convulsions               | 163| 54.3|
|Mental state              | 141| 47.0|
|Other symptoms            | 144| 48.0|



Cases by lab results 
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// describe_by_labs \\\
--------------------------------------------------------------------------------


This chunk gives the counts and proportions for all of the variables in LABS.
If you do not have lab results, comment out this chunk.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(LABS)` instead of `LABS` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
## mutate: converted 'variable' from factor to character (0 new NA)
## 
## rename: renamed one variable (Lab test)
```



|Lab test                          | Positive (n)|    %| Negative (n)|    %| Not done (n)|    %|
|:---------------------------------|------------:|----:|------------:|----:|------------:|----:|
|Hep b rdt                         |          104| 34.7|          104| 34.7|           92| 30.7|
|Hep c rdt                         |           92| 30.7|          101| 33.7|          107| 35.7|
|Hep e rdt                         |          104| 34.7|           95| 31.7|          101| 33.7|
|Test hepatitis a                  |          108| 36.0|           89| 29.7|          103| 34.3|
|Test hepatitis b                  |           98| 32.7|           99| 33.0|          103| 34.3|
|Test hepatitis c                  |          107| 35.7|           89| 29.7|          104| 34.7|
|Test hepatitis e igg              |           86| 28.7|          120| 40.0|           94| 31.3|
|Test hepatitis e igm              |           94| 31.3|           89| 29.7|          117| 39.0|
|Test hepatitis e genotype         |          101| 33.7|           88| 29.3|          111| 37.0|
|Test hepatitis e virus            |          112| 37.3|          100| 33.3|           88| 29.3|
|Malaria rdt at admission          |          109| 36.3|           88| 29.3|          103| 34.3|
|Malaria blood film                |           88| 29.3|          105| 35.0|          107| 35.7|
|Dengue                            |           98| 32.7|          102| 34.0|          100| 33.3|
|Dengue rdt                        |          106| 35.3|           86| 28.7|          108| 36.0|
|Yellow fever                      |          102| 34.0|           98| 32.7|          100| 33.3|
|Typhoid                           |           97| 32.3|           93| 31.0|          110| 36.7|
|Chikungunya onyongnyong           |          110| 36.7|           98| 32.7|           92| 30.7|
|Ebola marburg                     |           82| 27.3|          129| 43.0|           89| 29.7|
|Lassa fever                       |          100| 33.3|           94| 31.3|          106| 35.3|
|Other arthropod transmitted virus |          104| 34.7|           93| 31.0|          103| 34.3|
|Other pathogen                    |           93| 31.0|           98| 32.7|          109| 36.3|




#### Case fatality ratio 

Of 154 (51.3%) inpatients, there have been 38 (12.7%) deaths, of which 
37 (12.3%) 
were dead on arrival. 

Among inpatients who died, the time to death is shown below. 
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// describe_time_to_death \\\
--------------------------------------------------------------------------------

If you have the time_to_death available, you can use this chunk to describe when
inpatients died. If not, comment it out.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

|Time (hours) | Deaths (n)|     %|
|:------------|----------:|-----:|
|0-4 hours    |          9|  23.7|
|>4-24 hours  |         12|  31.6|
|>24-48 hours |          9|  23.7|
|>48 hours    |          8|  21.1|
|Total        |         38| 100.0|



The case fatality ratio among inpatients with known outcomes is below.
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// overall_cfr \\\
--------------------------------------------------------------------------------

This chunk gives the case fatality ratio among inpatients with outcomes.
If you have no deaths reported, the table will not be useful.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

| Deaths| Cases| CFR (%)|95%CI          |
|------:|-----:|-------:|:--------------|
|     38|   154|    24.7|(18.54--32.05) |


The case fatality ratio by sex among inpatients with known outcomes is below. 
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// cfr_by_sex \\\
--------------------------------------------------------------------------------

This chunk gives the case fatality ratio among inpatients with outcomes, divided
by sex. If you have no deaths reported, the table will not be useful.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

|Sex    | Deaths| Cases| CFR (%)|95%CI          |
|:------|------:|-----:|-------:|:--------------|
|Male   |     13|    56|    23.2|(14.10--35.77) |
|Female |     12|    51|    23.5|(14.00--36.76) |
|-      |     13|    47|    27.7|(16.94--41.76) |
|Total  |     38|   154|    24.7|(18.54--32.05) |



CFR by age group among inpatients with known outcomes
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// cfr_by_age_group \\\
--------------------------------------------------------------------------------

This chunk gives the case fatality ratio among inpatients with outcomes, divided
by age group. If you have no deaths reported, the table will not be useful.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

|Age group | Deaths| Cases| CFR (%)|95%CI          |
|:---------|------:|-----:|-------:|:--------------|
|0-2       |      1|     6|    16.7|(3.01--56.35)  |
|3-14      |      8|    27|    29.6|(15.85--48.48) |
|15-29     |      7|    37|    18.9|(9.48--34.20)  |
|30-44     |      7|    36|    19.4|(9.75--35.03)  |
|45+       |     15|    48|    31.2|(19.95--45.33) |
|Total     |     38|   154|    24.7|(18.54--32.05) |


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// cfr_by_case_def \\\
--------------------------------------------------------------------------------

This chunk gives the case fatality ratio among inpatients with outcomes, divided
by case definition. You must have lab results or a case definition defined to
use this. If you have no deaths, the table will not be useful.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->




#### Attack rate

The attack rate per 10,000 population is below (based on available population data available for the catchment area/region of interest). 



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// collect_variables \\\
--------------------------------------------------------------------------------

This defines your population by summing up the population by age group.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->




Below gives the attack rate per 10,000 population (N = 4,999.5)

<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
///attack_rate \\\
--------------------------------------------------------------------------------

This chunk calculates the attack rate and then shows you the confidence
intervals.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

```
## rename: renamed 4 variables (Cases (n), Population, AR (per 10,000), 95%CI)
```

```
## select: dropped one variable (Population)
```



| Cases (n)| AR (per 10,000)|            95%CI|
|---------:|---------------:|----------------:|
|       300|           600.1| (537.54--669.33)|

Here, we can see that the attack rate for a population of 4,999.5 was 600.06 (CI 537.54--669.33).

To give attack rate by age group, with appropriate population denominators, use the following code. 



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// attack_rate_by_agegroup \\\
--------------------------------------------------------------------------------

This chunk calculates the attack rate by age group and then gives a table of
attack rate by group.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

```
## count: now 5 rows and 2 columns, ungrouped
```

```
## left_join: added 2 columns (proportions, population)
```

```
##            > rows only in x   0
```

```
##            > rows only in y  (0)
```

```
##            > matched rows     5
```

```
##            >                 ===
```

```
##            > rows total       5
```

```
## select: dropped 3 variables (n, proportions, population)
```

```
## rename: renamed 5 variables (Age group, Cases (n), Population, AR (per 10,000), 95%CI)
```



|Age group | Cases (n)| Population| AR (per 10,000)|95%CI              |
|:---------|---------:|----------:|---------------:|:------------------|
|0-2       |        16|      340.0|           470.6|(291.71--750.67)   |
|3-14      |        63|    1,811.0|           347.9|(272.84--442.60)   |
|15-29     |        73|    1,380.0|           529.0|(422.82--659.98)   |
|30-44     |        55|      808.0|           680.7|(526.68--875.59)   |
|45+       |        93|      660.5|         1,408.0|(1163.49--1694.10) |



#### Mortality attributable to AJS
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This section can only be used if you are in a closed population (eg refugee camp).
The assumptions don't hold in an open/community setting.

This chunk calculates the attack rate by age group and then gives a table of
attack rate by group.

This section gives mortality rates attributable to AJS in a closed population. 
It does not calculate all-cause mortality. It assumes that all AJS deaths are 
among inpatients.

This demonstrates three ways of calculating mortality rate based on 
catchment population (twice) and based on hospital population. 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// collect_variables_rates \\\
--------------------------------------------------------------------------------

This chunk calculates key variables to do mortality rate.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

```
## filter: no rows removed
```

```
## summarise: now one row and one column, ungrouped
```



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// mortality_rate \\\
--------------------------------------------------------------------------------

To produce a mortality rate (attribuatble to AJS) per 10,000 people use the
following code chunk. This assumes that you are capturing every AJS death in
your population.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Mortality rate attributable to AJS per 10,000 population

```
## rename: renamed 4 variables (Deaths, Population, Mortality (per 10,000), 95%CI)
```



| Deaths| Population| Mortality (per 10,000)|95%CI            |
|------:|----------:|----------------------:|:----------------|
|     84|     4999.5|                    168|(135.92--207.53) |



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// mortality_rate_CMR \\\
--------------------------------------------------------------------------------

To produce a crude mortality rate attributable to AJS per 10,000 people per 
day, use the following code chunk. 

This assumes that you are capturing every AJS death in your population and 
that your population remains stable over the time period of interest. 

In this situation the time period of interest is from the beginning of the 
epiweek your first case occurred in, until the last day of the epiweek you are 
currently reporting on. (see this presentation (https://www.odi.org/sites/odi.org.uk/files/odi-assets/events-presentations/776.pdf) for more detail)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Crude mortality rate attributable to AJS per 10,000 population per day


```
## rename: renamed 4 variables (Deaths, Person-days, Mortality (per 10,000/day), 95%CI)
```



| Deaths| Person-days| Mortality (per 10,000/day)|95%CI        |
|------:|-----------:|--------------------------:|:------------|
|     84|     2654735|                        0.3|(0.26--0.39) |



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// mortality_rate_patients \\\
--------------------------------------------------------------------------------

Alternatively, if you are unsure whether your hospital deaths are
representative of the wider population, use the following code chunk. 
This uses the person days of cases in your linelist with a known outcome. 
However, this will give you an unreasonably high mortality rate, as 
those in hospital will only be the most severely affected. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Mortality rate attributable to AJS per 10,000 patients per day


```
## rename: renamed 4 variables (Deaths, Person-days, Mortality (per 10,000/day), 95%CI)
```



| Deaths| Person-days| Mortality (per 10,000/day)|95%CI           |
|------:|-----------:|--------------------------:|:---------------|
|     84|        9533|                       88.1|(71.23--108.95) |






### Time
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This section focuses on the when of the outbreak:
- When did cases fall ill?
- Are numbers increasing or decreasing?

There is code to include an epi curve.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

There were 0 (0.0%) cases missing dates of onset.

<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// epicurve \\\
--------------------------------------------------------------------------------

# This chunk will calculate weekly incidence and plot your epicurve.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
![](skeleton_files/figure-docx/epicurve-1.png)<!-- -->

The peak of the outbreak was in 2018 W12  




<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// biweekly_epicurve \\\
--------------------------------------------------------------------------------

If needed, you can plot a biweekly epicurve.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

![](skeleton_files/figure-docx/monthly_epicurve-1.png)<!-- -->



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// incidence_by_gender \\\
--------------------------------------------------------------------------------

You can plot weekly incidence by gender using this chunk.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

![](skeleton_files/figure-docx/incidence_by_gender-1.png)<!-- -->



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// incidence_by_case_def \\\
--------------------------------------------------------------------------------

This chunk shows the weekly incidence by case definition. You must have a
defined case definition variable.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
![](skeleton_files/figure-docx/incidence_by_case_def-1.png)<!-- -->



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// incidence_by_water_source \\\
--------------------------------------------------------------------------------

You can similarly stratify your incidence by water source (or any other
categorical variable!).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

![](skeleton_files/figure-docx/incidence_by_water_source-1.png)<!-- -->



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// incidence_by_sex_facility \\\
--------------------------------------------------------------------------------

This chunk limits the data to only inpatients, then stratifies by sex.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Cases by week of onset among inpatients by sex 

![](skeleton_files/figure-docx/incidence_by_sex_facility-1.png)<!-- -->




<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// attack_rate_per_week \\\
--------------------------------------------------------------------------------

This chunk creates a table of attack rate by week
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Attack rate per 10,000 population by week 


|  Epiweek| Cases (n)| Population| AR (per 10,000)|          95%CI|
|--------:|---------:|----------:|---------------:|--------------:|
| 2018 W02|         1|     4999.5|               2|  (0.35--11.32)|
| 2018 W03|         2|     4999.5|               4|  (1.10--14.58)|
| 2018 W04|         2|     4999.5|               4|  (1.10--14.58)|
| 2018 W05|         7|     4999.5|              14|  (6.78--28.88)|
| 2018 W06|         8|     4999.5|              16|  (8.11--31.55)|
| 2018 W07|        11|     4999.5|              22| (12.29--39.36)|
| 2018 W08|         9|     4999.5|              18|  (9.47--34.18)|
| 2018 W09|        12|     4999.5|              24| (13.74--41.91)|
| 2018 W10|        17|     4999.5|              34| (21.24--54.39)|
| 2018 W11|        21|     4999.5|              42| (27.49--64.13)|
| 2018 W12|        29|     4999.5|              58| (40.42--83.18)|
| 2018 W13|        29|     4999.5|              58| (40.42--83.18)|
| 2018 W14|        25|     4999.5|              50| (33.89--73.72)|
| 2018 W15|        28|     4999.5|              56| (38.78--80.83)|
| 2018 W16|        21|     4999.5|              42| (27.49--64.13)|
| 2018 W17|        27|     4999.5|              54| (37.14--78.46)|
| 2018 W18|        15|     4999.5|              30| (18.19--49.45)|
| 2018 W19|        21|     4999.5|              42| (27.49--64.13)|
| 2018 W20|        15|     4999.5|              30| (18.19--49.45)|
 


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// cumulative_attack_rate_per_week \\\
--------------------------------------------------------------------------------

This chunk calculates the cumulative attack rate per week.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Cumulative attack rate per 10,000 population per week


```
## select: dropped 2 variables (n, cumulative)
```

```
## rename: renamed 5 variables (Epiweek, Cases (n), Population, AR (per 10,000), 95%CI)
```



|  Epiweek| Cases (n)| Population| AR (per 10,000)|            95%CI|
|--------:|---------:|----------:|---------------:|----------------:|
| 2018 W02|         1|     4999.5|             2.0|    (0.35--11.32)|
| 2018 W03|         3|     4999.5|             6.0|    (2.04--17.63)|
| 2018 W04|         5|     4999.5|            10.0|    (4.27--23.39)|
| 2018 W05|        12|     4999.5|            24.0|   (13.74--41.91)|
| 2018 W06|        20|     4999.5|            40.0|   (25.91--61.71)|
| 2018 W07|        31|     4999.5|            62.0|   (43.72--87.88)|
| 2018 W08|        40|     4999.5|            80.0|  (58.81--108.76)|
| 2018 W09|        52|     4999.5|           104.0|  (79.41--136.13)|
| 2018 W10|        69|     4999.5|           138.0| (109.21--174.29)|
| 2018 W11|        90|     4999.5|           180.0| (146.69--220.75)|
| 2018 W12|       119|     4999.5|           238.0| (199.28--284.08)|
| 2018 W13|       148|     4999.5|           296.0| (252.54--346.74)|
| 2018 W14|       173|     4999.5|           346.0| (298.84--400.38)|
| 2018 W15|       201|     4999.5|           402.0| (351.03--460.12)|
| 2018 W16|       222|     4999.5|           444.0| (390.36--504.73)|
| 2018 W17|       249|     4999.5|           498.0| (441.13--561.88)|
| 2018 W18|       264|     4999.5|           528.1| (469.42--593.55)|
| 2018 W19|       285|     4999.5|           570.1| (509.12--637.79)|
| 2018 W20|       300|     4999.5|           600.1| (537.54--669.33)|



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// cumulative_attack_rate_per_week \\\
--------------------------------------------------------------------------------

This chunk calculates the CFR among inpatients as a proportion per week.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Case fatality ratio as a proportion among inpatients by week


```
## Error in `chr_as_locations()`:
## ! Can't rename columns that don't exist.
## x Column `epiweek` doesn't exist.
```



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You could plot the AR (in the population) and CFR (among inpatients only)
together with the epicurve by epiweek.

We will do this in three steps: 
- creating the AR graph (ar_line_graph)
- creating the CFR graph (cfr_line_graph)
- combining and plotting 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// ar_line_graph \\\
--------------------------------------------------------------------------------

This chunk sets up the AR graph. (It does not print the graph automatically.)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->




<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// cfr_line_graph \\\
--------------------------------------------------------------------------------

This chunk sets up the CFR graph. (It does not print the graph automatically.)
If you do not have any deaths in your data set, this will not show anything.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->




<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// epicurve_ar_cfr \\\
--------------------------------------------------------------------------------

This chunk prints the AR and CFR with the epicurve.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
![](skeleton_files/figure-docx/epicurve_ar_cfr-1.png)<!-- -->



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// describe_admissions_by_epiweek \\\
--------------------------------------------------------------------------------

This chunk creates a table showing inpatient admissions by  case definition 
and epiweek.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Inpatient admissions by case definition and week  


```
## filter: removed 146 rows (49%), 154 rows remaining
```

```
## select: dropped one variable (variable)
```



|value    | Confirmed (n)|     %| Probable (n)|     %| Suspected (n)|     %| Total|
|:--------|-------------:|-----:|------------:|-----:|-------------:|-----:|-----:|
|2018 W03 |             0|   0.0|            0|   0.0|             1|   1.7|     1|
|2018 W05 |             0|   0.0|            0|   0.0|             3|   5.1|     3|
|2018 W06 |             0|   0.0|            3|   6.8|             2|   3.4|     5|
|2018 W07 |             1|   2.0|            1|   2.3|             2|   3.4|     4|
|2018 W08 |             0|   0.0|            1|   2.3|             3|   5.1|     4|
|2018 W09 |             3|   5.9|            3|   6.8|             2|   3.4|     8|
|2018 W10 |             4|   7.8|            2|   4.5|             4|   6.8|    10|
|2018 W11 |             6|  11.8|            2|   4.5|             6|  10.2|    14|
|2018 W12 |             6|  11.8|            6|  13.6|             3|   5.1|    15|
|2018 W13 |             5|   9.8|            9|  20.5|             6|  10.2|    20|
|2018 W14 |             4|   7.8|            3|   6.8|             3|   5.1|    10|
|2018 W15 |             4|   7.8|            1|   2.3|             6|  10.2|    11|
|2018 W16 |             3|   5.9|            4|   9.1|             3|   5.1|    10|
|2018 W17 |             3|   5.9|            4|   9.1|             6|  10.2|    13|
|2018 W18 |             4|   7.8|            2|   4.5|             1|   1.7|     7|
|2018 W19 |             4|   7.8|            1|   2.3|             6|  10.2|    11|
|2018 W20 |             4|   7.8|            2|   4.5|             2|   3.4|     8|
|Total    |            51| 100.0|           44| 100.0|            59| 100.0|   154|



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// describe_admissions_by_epiweek \\\
--------------------------------------------------------------------------------

This chunk creates a table showing inpatient exits by type per epiweek.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Inpatient discharges by reason for exit and week 


```
## filter: removed 146 rows (49%), 154 rows remaining
```

```
## select: dropped one variable (variable)
```

```
## rename: renamed one variable (Week)
```



|Week     | Died (n)|     %| Discharged (n)|     %| Left (n)|     %| Transferred (n)|     %| Total|
|:--------|--------:|-----:|--------------:|-----:|--------:|-----:|---------------:|-----:|-----:|
|2018 W03 |        0|   0.0|              0|   0.0|        0|   0.0|               1|   1.8|     1|
|2018 W05 |        0|   0.0|              0|   0.0|        1|   3.1|               2|   3.6|     3|
|2018 W06 |        1|   2.6|              1|   3.4|        1|   3.1|               2|   3.6|     5|
|2018 W07 |        3|   7.9|              0|   0.0|        0|   0.0|               1|   1.8|     4|
|2018 W08 |        0|   0.0|              2|   6.9|        1|   3.1|               1|   1.8|     4|
|2018 W09 |        2|   5.3|              0|   0.0|        3|   9.4|               3|   5.5|     8|
|2018 W10 |        3|   7.9|              1|   3.4|        1|   3.1|               5|   9.1|    10|
|2018 W11 |        6|  15.8|              1|   3.4|        3|   9.4|               4|   7.3|    14|
|2018 W12 |        5|  13.2|              2|   6.9|        4|  12.5|               4|   7.3|    15|
|2018 W13 |        4|  10.5|              8|  27.6|        2|   6.2|               6|  10.9|    20|
|2018 W14 |        3|   7.9|              3|  10.3|        1|   3.1|               3|   5.5|    10|
|2018 W15 |        1|   2.6|              2|   6.9|        3|   9.4|               5|   9.1|    11|
|2018 W16 |        2|   5.3|              2|   6.9|        3|   9.4|               3|   5.5|    10|
|2018 W17 |        3|   7.9|              2|   6.9|        3|   9.4|               5|   9.1|    13|
|2018 W18 |        1|   2.6|              2|   6.9|        3|   9.4|               1|   1.8|     7|
|2018 W19 |        4|  10.5|              1|   3.4|        2|   6.2|               4|   7.3|    11|
|2018 W20 |        0|   0.0|              2|   6.9|        1|   3.1|               5|   9.1|     8|
|Total    |       38| 100.0|             29| 100.0|       32| 100.0|              55| 100.0|   154|






### Place 
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This section focuses on the where of the outbreak: what area is affected, how
many villages, and so on.

There is code to include maps based on distribution of cases. You must have a
shapefile to create this map.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->


#### Descriptive



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// describe_by_region_facility \\\
--------------------------------------------------------------------------------

This chunk creates a basic table of the number and percent of cases by region
and by facility. If you only have one facility, you can remove the strata.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Cases by region and facility type


```
## select: dropped one variable (variable)
```

```
## rename: renamed one variable (Region)
```



|Region    | Outpatient (n)|     %| Inpatient (n)|     %| Total|
|:---------|--------------:|-----:|-------------:|-----:|-----:|
|Village A |             47|  32.2|            41|  26.6|    88|
|Village B |             36|  24.7|            40|  26.0|    76|
|Village C |             29|  19.9|            31|  20.1|    60|
|Village D |             34|  23.3|            42|  27.3|    76|
|Total     |            146| 100.0|           154| 100.0|   300|

  

<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// describe_by_region_outcome \\\
--------------------------------------------------------------------------------

This chunk creates a basic table of the outcomes among inpatients by region.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Inpatient discharges by reason for exit and region 


```
## filter: removed 146 rows (49%), 154 rows remaining
```

```
## select: dropped one variable (variable)
```

```
## rename: renamed one variable (Region)
```



|Region    | Died (n)|     %| Discharged (n)|     %| Left (n)|     %| Transferred (n)|     %| Total|
|:---------|--------:|-----:|--------------:|-----:|--------:|-----:|---------------:|-----:|-----:|
|Village A |       10|  26.3|              8|  27.6|        8|  25.0|              15|  27.3|    41|
|Village B |        8|  21.1|             10|  34.5|       10|  31.2|              12|  21.8|    40|
|Village C |        5|  13.2|              4|  13.8|        7|  21.9|              15|  27.3|    31|
|Village D |       15|  39.5|              7|  24.1|        7|  21.9|              13|  23.6|    42|
|Total     |       38| 100.0|             29| 100.0|       32| 100.0|              55| 100.0|   154|



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// attack_rate_by_region \\\
--------------------------------------------------------------------------------

If you do not have a shapefile, you may want to calculate attack rates by
region.

Consider facet wrapping by a larger unit if you have many regions (eg if you
have patients from 10+ villages, you may want to show your tables by health
zone)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Attack rage per 10,000 population by region 


```
## count: now 4 rows and 2 columns, ungrouped
```

```
## left_join: added 2 columns (proportions, population)
```

```
##            > rows only in x   0
```

```
##            > rows only in y  (0)
```

```
##            > matched rows     4
```

```
##            >                 ===
```

```
##            > rows total       4
```

```
## select: dropped 3 variables (n, proportions, population)
```

```
## rename: renamed 6 variables (Region, Cases (n), Population, AR (per 10,000), Lower 95%CI, …)
```

```
## rename: renamed one variable (95%CI)
```



|    Region| Cases (n)| Population| AR (per 10,000)|             95%CI|
|---------:|---------:|----------:|---------------:|-----------------:|
| Village A|        88|      1,105|           796.4|  (650.93--970.96)|
| Village B|        76|        870|           873.6| (703.62--1079.79)|
| Village C|        60|      1,775|           338.0|  (263.51--432.68)|
| Village D|        76|      1,225|           620.4|  (498.53--769.67)|


<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// bar_attack_rate_by_region \\\
--------------------------------------------------------------------------------

You can then plot the above table on a bar plot. AR is on the y-axis, and it
will show regions in descending order by AR.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
![](skeleton_files/figure-docx/bar_attack_rate_by_region-1.png)<!-- -->



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// mortality_rate_region \\\
--------------------------------------------------------------------------------

You could also calculate mortality rate by region (check the mortality code
chunk in Person section for assumptions).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
Mortality rate per 10,000 population by region 


```
## group_by: one grouping variable (patient_origin)
```

```
## filter (grouped): removed 216 rows (72%), 84 rows remaining
```

```
## summarise: now 4 rows and 2 columns, ungrouped
```

```
## left_join: added 2 columns (proportions, population)
```

```
##            > rows only in x   0
```

```
##            > rows only in y  (0)
```

```
##            > matched rows     4
```

```
##            >                 ===
```

```
##            > rows total       4
```

```
## select: dropped 3 variables (deaths, proportions, population)
```

```
## rename: renamed 5 variables (Region, Deaths, Population, Mortality (per 10,000), 95%CI)
```



|Region    | Deaths| Population| Mortality (per 10,000)|95%CI            |
|:---------|------:|----------:|----------------------:|:----------------|
|Village A |     24|       1105|                  217.2|(146.38--321.15) |
|Village B |     21|        870|                  241.4|(158.41--366.19) |
|Village C |     14|       1775|                   78.9|(47.04--131.96)  |
|Village D |     25|       1225|                  204.1|(138.61--299.54) |




#### Maps 
<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// read_shapefiles \\\
--------------------------------------------------------------------------------

To create maps, you need to have a shapefile of the area. Often, the MSF GIS
unit can provide shapefiles.

Your shapefile can be a polygon or points. Polygons do not need to be contiguous.

The names of the polygons or points MUST match the names in your linelist.

Your coordinate reference system needs to be WGS84.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->




<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// dot_map \\\
--------------------------------------------------------------------------------

If you have the coordinates for your cases, you can create a dot map showing
where the cases are.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->




<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// choropleth_maps \\\
--------------------------------------------------------------------------------

Once you have loaded your shapefile, you can map the case counts or attack rates.

You will want to make your counts or AR categorical. R will not do this
automatically (unlike QGIS or ArcGIS).

This chunk will walk you through several steps:
- Create table with categories of counts or ARs by region.
- Join your table with your shapefile.
- Choose which variable you will use.

Make sure you delete or comment out the section you are not using.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
![](skeleton_files/figure-docx/choropleth_maps-1.png)<!-- -->



<!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// map_for_loop_epiweek \\\
--------------------------------------------------------------------------------

This chunk will create a map and barplot by week.

You need to:
- Decide if you want to show counts, AR, or categories of counts/AR.
- Define categories for your variable.
- Replace `Cases (n)` and `AR (per 10,000)` or "categories", appropriately.


If you have a lot of map regions, you may want to use facet_wrap to show
sub-units.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
![](skeleton_files/figure-docx/map_for_loop_epiweek-1.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-2.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-3.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-4.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-5.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-6.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-7.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-8.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-9.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-10.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-11.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-12.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-13.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-14.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-15.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-16.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-17.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-18.png)<!-- -->![](skeleton_files/figure-docx/map_for_loop_epiweek-19.png)<!-- -->

