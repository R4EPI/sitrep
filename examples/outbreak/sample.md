Outbreak report
================

This is a test document to gather functions and code snippets that
eventually will evolve into an outbreak report template (+ package(s)).

### Person

  - \[Who is affected: how many in total; male or female; young, adult
    or old? What are the links between affected people – work place,
    school, social gathering? Is there a high rate of illness in
    contacts? Is there a high rate of illness in health workers? You may
    want to include: a bar chart showing case numbers or incidence by
    age group and sex; attack rates (AR); and numbers of deaths (in
    suspected and confirmed cases), mortality rates and/or case fatality
    ratio (CFR)\]

In total there were 136 cases. There were 39 females affected and 95
males.

The most affected age group was 30-49 years.

#### Age

Cases by sex

| sex    |  n | prop |
| :----- | -: | ---: |
| Female | 39 | 28.7 |
| Male   | 95 | 69.9 |
| NA     |  2 |  1.5 |

Cases by age group

| age\_group |  n | prop |
| :--------- | -: | ---: |
| 0-4        |  5 |  3.7 |
| 5-9        |  8 |  5.9 |
| 10-29      | 46 | 33.8 |
| 30-49      | 56 | 41.2 |
| 50-79      | 21 | 15.4 |

Cases by age group and
definition

| age\_group | Confirmed\_n | Confirmed\_prop | Possible\_n | Possible\_prop | Probable\_n | Probable\_prop |
| :--------- | -----------: | --------------: | ----------: | -------------: | ----------: | -------------: |
| 0-4        |            3 |             4.4 |           1 |            2.9 |           1 |            3.0 |
| 5-9        |            6 |             8.8 |           0 |            0.0 |           2 |            6.1 |
| 10-29      |           25 |            36.8 |          11 |           31.4 |          10 |           30.3 |
| 30-49      |           28 |            41.2 |          16 |           45.7 |          12 |           36.4 |
| 50-79      |            6 |             8.8 |           7 |           20.0 |           8 |           24.2 |

Cases by age group and
sex

| age\_group | Female\_n | Female\_prop | Male\_n | Male\_prop | NA\_n | NA\_prop |
| :--------- | --------: | -----------: | ------: | ---------: | ----: | -------: |
| 0-4        |         1 |          2.6 |       2 |        2.1 |     2 |      100 |
| 5-9        |         4 |         10.3 |       4 |        4.2 |     0 |        0 |
| 10-29      |        12 |         30.8 |      34 |       35.8 |     0 |        0 |
| 30-49      |        17 |         43.6 |      39 |       41.1 |     0 |        0 |
| 50-79      |         5 |         12.8 |      16 |       16.8 |     0 |        0 |

Age pyramid

![](sample_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

CFR

The case fatality ratio among those with known outcomes is below

| deaths | population |  cfr | lower | upper |
| -----: | ---------: | ---: | ----: | ----: |
|     32 |         79 | 40.5 |  30.4 |  51.5 |

CFR by age group

| age\_group | deaths | population |  cfr | lower | upper |
| :--------- | -----: | ---------: | ---: | ----: | ----: |
| 50-79      |      7 |         12 | 58.3 |  32.0 |  80.7 |
| 30-49      |     14 |         29 | 48.3 |  31.4 |  65.6 |
| 10-29      |      8 |         27 | 29.6 |  15.9 |  48.5 |
| 5-9        |      2 |          6 | 33.3 |   9.7 |  70.0 |
| 0-4        |      1 |          5 | 20.0 |   3.6 |  62.4 |

#### Attack rate

The attack rate among per 100,000 population is below - based on
available population data for the whole country.

| cases | population |   ar | lower | upper |
| ----: | ---------: | ---: | ----: | ----: |
|   136 |     612202 | 22.2 |  18.8 |  26.3 |

The below gives the attack rate per week.

| cases | population |  ar | lower | upper |
| ----: | ---------: | --: | ----: | ----: |
|     1 |     612202 | 0.2 |   0.0 |   0.9 |
|     1 |     612202 | 0.2 |   0.0 |   0.9 |
|     3 |     612202 | 0.5 |   0.2 |   1.4 |
|     2 |     612202 | 0.3 |   0.1 |   1.2 |
|     7 |     612202 | 1.1 |   0.6 |   2.4 |
|    18 |     612202 | 2.9 |   1.9 |   4.6 |
|    28 |     612202 | 4.6 |   3.2 |   6.6 |
|    37 |     612202 | 6.0 |   4.4 |   8.3 |
|    19 |     612202 | 3.1 |   2.0 |   4.8 |
|     5 |     612202 | 0.8 |   0.3 |   1.9 |
|     2 |     612202 | 0.3 |   0.1 |   1.2 |
|     1 |     612202 | 0.2 |   0.0 |   0.9 |
|     1 |     612202 | 0.2 |   0.0 |   0.9 |
|     1 |     612202 | 0.2 |   0.0 |   0.9 |
|    10 |     612202 | 1.6 |   0.9 |   3.0 |

The below gives the cummulative attack rate per week.

| cases | population |   ar | lower | upper |
| ----: | ---------: | ---: | ----: | ----: |
|     1 |     612202 |  0.2 |   0.0 |   0.9 |
|     2 |     612202 |  0.3 |   0.1 |   1.2 |
|     5 |     612202 |  0.8 |   0.3 |   1.9 |
|     7 |     612202 |  1.1 |   0.6 |   2.4 |
|    14 |     612202 |  2.3 |   1.4 |   3.8 |
|    32 |     612202 |  5.2 |   3.7 |   7.4 |
|    60 |     612202 |  9.8 |   7.6 |  12.6 |
|    97 |     612202 | 15.8 |  13.0 |  19.3 |
|   116 |     612202 | 18.9 |  15.8 |  22.7 |
|   121 |     612202 | 19.8 |  16.5 |  23.6 |
|   123 |     612202 | 20.1 |  16.8 |  24.0 |
|   124 |     612202 | 20.3 |  17.0 |  24.1 |
|   125 |     612202 | 20.4 |  17.1 |  24.3 |
|   126 |     612202 | 20.6 |  17.3 |  24.5 |
|   136 |     612202 | 22.2 |  18.8 |  26.3 |

#### Mortality

Mortality rate per 100,000:

| deaths | population | mortality per 10 000 | lower | upper |
| -----: | ---------: | -------------------: | ----: | ----: |
|     32 |     612202 |                  0.5 |   0.4 |   0.7 |

#### 2x2 tables

    ## Warning in univariate_analysis(measure = "OR", digits = 3, outcome =
    ## outcome, : Removed 2 rows due to missing values

| exposure  | exp\_cases | unexp\_cases | cases\_odds | exp\_noncases | unexp\_noncases | noncases\_odds | estimate | lower | upper | p\_value |
| :-------- | ---------: | -----------: | ----------: | ------------: | --------------: | -------------: | -------: | ----: | ----: | -------: |
| is\_male  |         22 |           34 |       0.647 |             9 |              12 |           0.75 |    0.863 | 0.312 | 2.386 |    0.779 |
| is\_child |          3 |           11 |       0.273 |            28 |              35 |           0.80 |    0.341 | 0.087 | 1.341 |    0.123 |

### Time

  - \[When did the cases fall ill? Are numbers increasing or stable? You
    may want to include an Epi curve (bar chart showing number of new
    (suspected and confirmed) cases each day/week) \]

There were 10 cases missing dates of onset.

The peak of the outbreak was in 2013-W15.

![](sample_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

You may also want to stratify by gender.

![](sample_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

### Place

  - \[Across what area: one or several villages, all from same school,
    etc. You may want to include a map of the distribution of cases;
    attack rates by location\]

#### Quick and simple map with ggmap

Just WIP. Not sure if ggmap is a good approach.

DO THIS WITH ALL OF CHINA FOR A CHOROPLETH USING GADM SHAPEFILES ALSO
GET OSM TILES AND
DATA

![](sample_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

#### Mortality rate per district

| Province | Number of cases | Population | Incidence per 1000 | Lower 95% CI | Upper 95% CI |
| :------- | --------------: | ---------: | -----------------: | -----------: | -----------: |
| Anhui    |               2 |      43177 |               0.05 |         0.01 |         0.17 |
| Beijing  |               0 |      16297 |               0.00 |         0.00 |         0.24 |
| Fujian   |               0 |      53657 |               0.00 |         0.00 |         0.07 |
| Hebei    |               1 |      64536 |               0.02 |         0.00 |         0.09 |
| Henan    |               1 |      56740 |               0.02 |         0.00 |         0.10 |
| Hunan    |               1 |      60455 |               0.02 |         0.00 |         0.09 |
| Jiangsu  |               4 |      80308 |               0.05 |         0.02 |         0.13 |
| Jiangxi  |               1 |      19072 |               0.05 |         0.01 |         0.30 |
| Shandong |               0 |      33510 |               0.00 |         0.00 |         0.12 |
| Shanghai |              16 |      83121 |               0.19 |         0.12 |         0.31 |
| Zhejiang |               6 |      25206 |               0.24 |         0.11 |         0.52 |
