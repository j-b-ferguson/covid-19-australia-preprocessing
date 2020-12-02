---
title: my title
author: my name
date: today
...

<div class="container-fluid main-container">

<div id="header" class="fluid-row">

</div>

# **Executive Summary**

This **report** brings together data of worldwide COVID-19 cases and
Australian COVID-19 testing. Through a series of pre-processing steps,
the data is filtered for cases and tests relevant to Australian states.
It is then tidied, scanned, transformed, and analysed, as required.

To begin, the data sets are imported and saved. All variables and their
data types are inspected and their context understood. Following the
tidy data principles, the data frames are put into tidy format by
reshaping the data frames from wide to long format. Afterwards, proper
data type conversions are performed to ensure all types are logically
correct. Now in tidy format, with type conversions completed, the data
frames are joined by their common keys using a left join. Then
manipulating, the data set is filtered row-wise, three new columns are
inserted, including one mutation.

The first scan section identifies and rectifies errors, inconsistencies,
and missing values. Errors and inconsistencies include negative values
of daily cases and tests, and the introduction of infinite values, NaN,
and logical inconsistencies as part of the tidying step. Missing values
are identified during the scan process. Through a series of user-defined
functions, in combination with various imputation methods, the errors,
inconsistencies, and missing values are corrected, and the joined data
frame cleaned.

The second scan section involves subsetting and filtering of the joined
and tidy data frame. Using a multivariate outlier technique, outliers
associated with numeric variables are identified and removed. Then
selecting the variables of interest concerning daily tests and cases, a
Boxcox transformation is performed. A linear model is then created from
the transformed data, and plots with simple linear regression displayed.

</div>

<div id="required-packages" class="section level2">

# Packages

``` r
library(readr) # Read csv files
library(dplyr) # Data frame manipulation
library(tidyr) # Data frame reshape
library(Hmisc) # Imputation methods 
library(DMwR) # For regression statistics
library(mice) # For missing value identification
library(plot.matrix) # Graphics for matrix plotting
library(VIM) # k-Nearest Neighbour Imputation
library(ggplot2) # Data visualisation
library(scales) # Custom scales for plot axis
library(RColorBrewer) # Custom colour palettes for plots
library(svglite) # Export plots into SVG format
```

</div>

<div id="data" class="section level3">

# Data

The first data set `time_series_covid19_confirmed_global.csv` is a
time-series obtained from the COVID-19 GitHub repository of John Hopkins
Whiting School of Engineering, Centre for Systems Science and
Engineering. The data set gives the daily cumulative counts of COVID-19
cases in various countries and regions, as well as applicable provinces
and states. The data set is extracted from the below link by
right-clicking the ‘Raw’ icon and clicking ‘Save link as…’, this allows
the data set to be saved into CSV format on a local drive.

[John Hopkins COVID-19 Cases Data](https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv)

| Variable | Name               | Description                                                                                                                                   |
|----------|--------------------|-----------------------------------------------------------------------------------------------------------------------------------------------|
| 1        | Province/State     | Provinces, states and dependencies of the following countries: Australia, Canada, China, Netherlands, United Kingdom, France and Denmark      |
| 2        | Country/Region     | Countries and regions with COVID-19 cases                                                                                                     |
| 3        | Lat                | Latitude in degrees of Country/Region or corresponding Province/State (if applicable)                                                         |
| 4        | Long               | Longtitude in degrees of Country/Region or corresponding Province/State (if applicable)                                                       |
| 5 - 273  | 1/22/20 … 10/16/20 | Cumulative counts of daily COVID-19 cases in their corresponding Country/Region or Province/State (if applicable) from 1/22/20 until 10/16/20 |

``` r
cases <- read_csv('time_series_covid19_confirmed_global.csv')
head(cases, 3)
```

    ## # A tibble: 3 x 273
    ##   `Province/State` `Country/Region`   Lat  Long `1/22/20` `1/23/20` `1/24/20`
    ##   <chr>            <chr>            <dbl> <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 <NA>             Afghanistan       33.9 67.7          0         0         0
    ## 2 <NA>             Albania           41.2 20.2          0         0         0
    ## 3 <NA>             Algeria           28.0  1.66         0         0         0
    ## # ... with 266 more variables: `1/25/20` <dbl>, `1/26/20` <dbl>,
    ## #   `1/27/20` <dbl>, `1/28/20` <dbl>, `1/29/20` <dbl>, `1/30/20` <dbl>,
    ## #   `1/31/20` <dbl>, `2/1/20` <dbl>, `2/2/20` <dbl>, `2/3/20` <dbl>,
    ## #   `2/4/20` <dbl>, `2/5/20` <dbl>, `2/6/20` <dbl>, `2/7/20` <dbl>,
    ## #   `2/8/20` <dbl>, `2/9/20` <dbl>, `2/10/20` <dbl>, `2/11/20` <dbl>,
    ## #   `2/12/20` <dbl>, `2/13/20` <dbl>, `2/14/20` <dbl>, `2/15/20` <dbl>,
    ## #   `2/16/20` <dbl>, `2/17/20` <dbl>, `2/18/20` <dbl>, `2/19/20` <dbl>,
    ## #   `2/20/20` <dbl>, `2/21/20` <dbl>, `2/22/20` <dbl>, `2/23/20` <dbl>,
    ## #   `2/24/20` <dbl>, `2/25/20` <dbl>, `2/26/20` <dbl>, `2/27/20` <dbl>,
    ## #   `2/28/20` <dbl>, `2/29/20` <dbl>, `3/1/20` <dbl>, `3/2/20` <dbl>,
    ## #   `3/3/20` <dbl>, `3/4/20` <dbl>, `3/5/20` <dbl>, `3/6/20` <dbl>,
    ## #   `3/7/20` <dbl>, `3/8/20` <dbl>, `3/9/20` <dbl>, `3/10/20` <dbl>,
    ## #   `3/11/20` <dbl>, `3/12/20` <dbl>, `3/13/20` <dbl>, `3/14/20` <dbl>,
    ## #   `3/15/20` <dbl>, `3/16/20` <dbl>, `3/17/20` <dbl>, `3/18/20` <dbl>,
    ## #   `3/19/20` <dbl>, `3/20/20` <dbl>, `3/21/20` <dbl>, `3/22/20` <dbl>,
    ## #   `3/23/20` <dbl>, `3/24/20` <dbl>, `3/25/20` <dbl>, `3/26/20` <dbl>,
    ## #   `3/27/20` <dbl>, `3/28/20` <dbl>, `3/29/20` <dbl>, `3/30/20` <dbl>,
    ## #   `3/31/20` <dbl>, `4/1/20` <dbl>, `4/2/20` <dbl>, `4/3/20` <dbl>,
    ## #   `4/4/20` <dbl>, `4/5/20` <dbl>, `4/6/20` <dbl>, `4/7/20` <dbl>,
    ## #   `4/8/20` <dbl>, `4/9/20` <dbl>, `4/10/20` <dbl>, `4/11/20` <dbl>,
    ## #   `4/12/20` <dbl>, `4/13/20` <dbl>, `4/14/20` <dbl>, `4/15/20` <dbl>,
    ## #   `4/16/20` <dbl>, `4/17/20` <dbl>, `4/18/20` <dbl>, `4/19/20` <dbl>,
    ## #   `4/20/20` <dbl>, `4/21/20` <dbl>, `4/22/20` <dbl>, `4/23/20` <dbl>,
    ## #   `4/24/20` <dbl>, `4/25/20` <dbl>, `4/26/20` <dbl>, `4/27/20` <dbl>,
    ## #   `4/28/20` <dbl>, `4/29/20` <dbl>, `4/30/20` <dbl>, `5/1/20` <dbl>,
    ## #   `5/2/20` <dbl>, `5/3/20` <dbl>, ...

The second data set `Total tests.csv` is a time-series made available by
www.covid19data.com.au. The data set gives the daily cumulative counts
of COVID-19 tests in Australian provinces and states. The CSV data set
was extracted from the below link by clicking the ‘download data’ icon.

[Infogram COVID-19 Tests Data](https://e.infogram.com/_/3osqzRmYBiJsJafg79YC?parent_url=https%3A%2F%2Fwww-covid19data-com-au.filesusr.com%2Fhtml%2F2aed08_944ecbfd558f24812177bca5a8a74000.html&src=embed#)

| Variable | Name                                | Description                                                                                                   |
|----------|-------------------------------------|---------------------------------------------------------------------------------------------------------------|
| 1        | (left as blank)                     | Days in 2020 of format dd-month from 8-Mar until 18-Oct                                                       |
| 2 - 8    | NSW, VIC, QLD, SA, WA, TAS, NT, ACT | Cumulative counts of daily COVID-19 tests in NSW, VIC, QLD, SA, WA, TAS, NT, ACT (Australia) for a given date |

``` r
tests <- read_csv('Total tests.csv')
head(tests, 3)
```

    ## # A tibble: 3 x 9
    ##   X1      NSW   VIC   QLD    SA    WA   TAS    NT   ACT
    ##   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 08/03  8008    NA    NA    NA    NA    NA    NA    NA
    ## 2 09/03  8371    NA    NA    NA    NA    NA    NA    NA
    ## 3 10/03 10165    NA    NA    NA    NA    NA    NA    NA

Now, take note of both data sets. In their current formats there are no
common variables for these data sets to be joined by. The data sets must
first be reshaped into tidy format. This process is covered in detail
after both data sets have been transformed into long format and joined
by the `Date` and `Province/State` variables.

</div>

<div id="understand" class="section level4">

# Understand

Before both data sets can be changed to tidy format and reshaped, the
data structure must be understood, beginning with the object classes.

``` r
list(class(cases), class(tests))
```

    ## [[1]]
    ## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame" 
    ## 
    ## [[2]]
    ## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"

So, both data sets have correctly inherited the data.frame class. The
internal storage modes of these objects shall also be examined, and
confirmed as list type.

``` r
list(typeof(cases), typeof(tests))
```

    ## [[1]]
    ## [1] "list"
    ## 
    ## [[2]]
    ## [1] "list"

The next thing of interest are the dimensions of the data frames.

``` r
list(dim(cases), dim(tests))
```

    ## [[1]]
    ## [1] 267 273
    ## 
    ## [[2]]
    ## [1] 211   9

Next, the overall structure of the data is examined. This is normally
achieved by using the `str()` function, which allows each variable and
its corresponding data type to be checked. However, with the `cases`
data frame, there are 273 variables, so a few short cuts are taken to
reduce the output. Using the `sapply()` function, the data type of all
columns are inspected. A short cut to inspect the date variables is to
check for unique data types.

``` r
list(sapply(cases[,1:4], class), 
     sapply(cases[,5:273], class) %>% 
       unique() %>% paste0('Variables 1/22/20 ... 10/16/20 are all ', .))
```

    ## [[1]]
    ## Province/State Country/Region            Lat           Long 
    ##    "character"    "character"      "numeric"      "numeric" 
    ## 
    ## [[2]]
    ## [1] "Variables 1/22/20 ... 10/16/20 are all numeric"

The structure of the `tests` data frame is checked as normal with
`str()`.

``` r
str(tests, give.attr = FALSE)
```

    ## tibble [211 x 9] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ X1 : chr [1:211] "08/03" "09/03" "10/03" "11/03" ...
    ##  $ NSW: num [1:211] 8008 8371 10165 10221 14856 ...
    ##  $ VIC: num [1:211] NA NA NA NA NA NA NA NA 11700 14200 ...
    ##  $ QLD: num [1:211] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ SA : num [1:211] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ WA : num [1:211] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ TAS: num [1:211] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ NT : num [1:211] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ ACT: num [1:211] NA NA NA NA NA NA NA NA NA NA ...

The next step is usually to apply the proper data type conversions.
However, it is more convienient in this case to reshape both data frames
into long format first.

<div id="reshape" class="section level5">

# Reshape

In this section, the tidy data principles are applied to both data
frames to enable further pre-processing. For refererence, the tidy data
principles are:

Condition 1 - Each variable is its own column,

Condition 2 - Each observation is its own row,

Condition 3 - Each value is its own cell.

Starting with the `cases` data frame, the dates that form the column
names from columns 5 to 273 are defined as variables, when infact they
are values and should be in cells of their own under a new variable
name, such as 'Date'. The values assigned to the date variables should
also be separate from the dates themselves, in a new variable such as
'Cumulative Cases'. Hence, the `cases` data frame is not in a tidy
format, and will require reshaping from wide to long format with
`gather()`.

Using `gather()`, the dates that currently form the variables will
instead be values under a new variable named `Date`. The values that
currently exist under each date variable will instead form values under
a new variable named `Cumulative Cases`.

``` r
cases_gathered <- gather(cases, key = 'Date', value = 'Cumulative Cases', 5:273)
head(cases_gathered, 3)
```

    ## # A tibble: 3 x 6
    ##   `Province/State` `Country/Region`   Lat  Long Date    `Cumulative Cases`
    ##   <chr>            <chr>            <dbl> <dbl> <chr>                <dbl>
    ## 1 <NA>             Afghanistan       33.9 67.7  1/22/20                  0
    ## 2 <NA>             Albania           41.2 20.2  1/22/20                  0
    ## 3 <NA>             Algeria           28.0  1.66 1/22/20                  0

Inspect now the `tests` data frame, the state names that form the column
names from columns 2 to 9 are defined as variables, when infact they are
values, and should be put into their own cells under a new variable
name, such as 'Province/State'. The values assigned to the state
variables should also be separate from the states themselves, under a
new variable such as 'Cumulative Tests'. Hence, the `tests` data frame
is in an untidy format, and will also required reshaping from wide to
long format.

Applying `gather()`, the states that currently form the variables will
instead be values under a new variable named `Province/State`. The
values that currently exist under each state variable will instead form
values under a new variable named `Cumulative Tests`. Note:
`Province/State` is now a variable common to both data frames.

``` r
tests_gathered <- tests %>% gather(key = 'Province/State', value = 'Cumulative Tests', 2:9)
head(tests_gathered, 3)
```

    ## # A tibble: 3 x 3
    ##   X1    `Province/State` `Cumulative Tests`
    ##   <chr> <chr>                         <dbl>
    ## 1 08/03 NSW                            8008
    ## 2 09/03 NSW                            8371
    ## 3 10/03 NSW                           10165

</div>

<div id="convert" class="section level6">

# Convert

Now that both data frames are in a tidy format, proper data type
conversions must be performed. Beginning wtih `tests_gathered`, the
column name of the first variable is changed from `X1` to `Date`. This
variable is common to both data frames.

``` r
colnames(tests_gathered)[1] <- 'Date'
names(tests_gathered)
```

    ## [1] "Date"             "Province/State"   "Cumulative Tests"

The format of `Date` is then changed into DD/MM/YY.

``` r
tests_gathered$Date <- replace(tests_gathered$Date, values = paste0(tests_gathered$Date, '/20'))
tests_gathered$Date[c(1,2,3)]
```

    ## [1] "08/03/20" "09/03/20" "10/03/20"

A type conversion from character to date is then easily performed.

``` r
tests_gathered$Date <- tests_gathered$Date %>% as.Date(., "%d/%m/%y")
```

Next, since the cumulative test numbers evolve dependent upon the
isolated province/state, it is logical to convert `Province/State` from
character type to factor.

``` r
tests_gathered$`Province/State` <- tests_gathered$`Province/State` %>% as.factor()
```

The converted `tests_gathered` data frame now has the following
structure.

``` r
str(tests_gathered)
```

    ## tibble [1,688 x 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Date            : Date[1:1688], format: "2020-03-08" "2020-03-09" ...
    ##  $ Province/State  : Factor w/ 8 levels "ACT","NSW","NT",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Cumulative Tests: num [1:1688] 8008 8371 10165 10221 14856 ...

Now consider `cases_gathered`, a subset of this data frame is taken with
variables of interest corresponding to Australian cases.

``` r
cases_gathered <- cases_gathered %>% filter(`Country/Region` == 'Australia') %>% 
  select(`Province/State`, Date, `Cumulative Cases`)
head(cases_gathered, 3)
```

    ## # A tibble: 3 x 3
    ##   `Province/State`             Date    `Cumulative Cases`
    ##   <chr>                        <chr>                <dbl>
    ## 1 Australian Capital Territory 1/22/20                  0
    ## 2 New South Wales              1/22/20                  0
    ## 3 Northern Territory           1/22/20                  0

The `Province/State` variable is then converted from character type to
factor, and the values are abbreviated to match those stored in
`tests_gathered`.

``` r
cases_gathered$`Province/State` <- cases_gathered$`Province/State`%>% as.factor() %>% 
  factor(levels = c('Australian Capital Territory', 'New South Wales', 'Northern Territory', 
                    'Queensland', 'South Australia', 'Tasmania', 'Victoria', 'Western Australia'), 
         labels = c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA'))
```

Data type conversion is then performed on the correctly formatted `date`
variable.

``` r
cases_gathered$Date <- cases_gathered$Date %>% as.Date(., "%m/%d/%y")
```

The converted `cases_gathered` data frame now has the following
structure.

``` r
str(cases_gathered)
```

    ## tibble [2,152 x 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Province/State  : Factor w/ 8 levels "ACT","NSW","NT",..: 1 2 3 4 5 6 7 8 1 2 ...
    ##  $ Date            : Date[1:2152], format: "2020-01-22" "2020-01-22" ...
    ##  $ Cumulative Cases: num [1:2152] 0 0 0 0 0 0 0 0 0 0 ...

</div>

<div id="join" class="section level7">

# Join

A join is now carried out to merge the data frames by the common
variables `Date` and `Province/State`. To decide join type, observe the
dates of the first non-zero instances below.

``` r
cases_gathered[which(cases_gathered$`Cumulative Cases` > 0)[1], ]
```

    ## # A tibble: 1 x 3
    ##   `Province/State` Date       `Cumulative Cases`
    ##   <fct>            <date>                  <dbl>
    ## 1 NSW              2020-01-26                  3

``` r
tests_gathered[which(tests_gathered$`Cumulative Tests` > 0)[1], ]
```

    ## # A tibble: 1 x 3
    ##   Date       `Province/State` `Cumulative Tests`
    ##   <date>     <fct>                         <dbl>
    ## 1 2020-03-08 NSW                            8008

The first cases and tests were recorded on `2020-01-26` and
`2020-03-08`, respectively. By virtue of logic, the first cases must
have been tested. So, the most sensible type of join is a left join of
`tests_gathered` onto `cases_gathered`. This will merge both datasets
from `2020-01-22`. Most notably, `Cumulative Tests` will have missing
values filled from this date until `2020-03-08`, but this is
unproblematic because these missing values will be imputed later.

``` r
covidAU_joined <- left_join(cases_gathered, tests_gathered, 
                            by = c("Province/State", "Date"))
head(covidAU_joined, 3)
```

    ## # A tibble: 3 x 4
    ##   `Province/State` Date       `Cumulative Cases` `Cumulative Tests`
    ##   <fct>            <date>                  <dbl>              <dbl>
    ## 1 ACT              2020-01-22                  0                 NA
    ## 2 NSW              2020-01-22                  0                 NA
    ## 3 NT               2020-01-22                  0                 NA

Note that the correct type conversions have remained after joining both
data frames.

``` r
str(covidAU_joined)
```

    ## tibble [2,152 x 4] (S3: tbl_df/tbl/data.frame)
    ##  $ Province/State  : Factor w/ 8 levels "ACT","NSW","NT",..: 1 2 3 4 5 6 7 8 1 2 ...
    ##  $ Date            : Date[1:2152], format: "2020-01-22" "2020-01-22" ...
    ##  $ Cumulative Cases: num [1:2152] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Cumulative Tests: num [1:2152] NA NA NA NA NA NA NA NA NA NA ...

Now finally, the newly joined data frame is filtered from the date of
the first recorded cases `2020-01-26`.

``` r
covidAU_joined <- covidAU_joined[covidAU_joined$`Cumulative Cases` > 0,]
head(covidAU_joined, 3)
```

    ## # A tibble: 3 x 4
    ##   `Province/State` Date       `Cumulative Cases` `Cumulative Tests`
    ##   <fct>            <date>                  <dbl>              <dbl>
    ## 1 NSW              2020-01-26                  3                 NA
    ## 2 VIC              2020-01-26                  1                 NA
    ## 3 NSW              2020-01-27                  4                 NA

</div>

</div>

<div id="mutate" class="section level3">

# Mutate

In this section, a new variable is mutated by considering `Daily Cases`
and `Daily Tests`. To begin, these two variables are created and filled
with missing values.

``` r
covidAU_joined$`Daily Cases` <- as.numeric(NA)
covidAU_joined$`Daily Tests` <- as.numeric(NA)
covidAU_joined %>% select(`Province/State`, Date, `Daily Cases`, `Daily Tests`) %>% head(3)
```

    ## # A tibble: 3 x 4
    ##   `Province/State` Date       `Daily Cases` `Daily Tests`
    ##   <fct>            <date>             <dbl>         <dbl>
    ## 1 NSW              2020-01-26            NA            NA
    ## 2 VIC              2020-01-26            NA            NA
    ## 3 NSW              2020-01-27            NA            NA

The presence of missing values is not problematic at this stage, as a
simple algorithm will be created later to calculate these values from
`Cumulative Cases` and `Cumulative Tests`. The benefit of creating these
new columns is such that they allow errors and inconsistencies to be
easily identified in `Cumulative Cases` and `Cumulative Tests`. Moving
on from this aside, a reasonable new variable to mutate from
`Daily Cases` and `Daily Tests` is the daily case to test ratio per
million. This column will initially be filled with missing values and
corrected after `Daily Cases` and `Daily Tests` have been properly
calculated. Hence, a function `mutatecovid()` has been created to ensure
reusable code to allow imputation of the
`Case to Test Ratio per Million`, as below.

``` r
mutatecovid <- function() {
  covidAU_joined <<- covidAU_joined %>% 
  mutate('Case to Test Ratio per Million' = ((covidAU_joined$`Daily Cases` / covidAU_joined$`Daily Tests`) * 1000000) 
    %>% round(0))
}
mutatecovid()
covidAU_joined %>% select(`Province/State`, Date, `Case to Test Ratio per Million`) %>% head(3)
```

    ## # A tibble: 3 x 3
    ##   `Province/State` Date       `Case to Test Ratio per Million`
    ##   <fct>            <date>                                <dbl>
    ## 1 NSW              2020-01-26                               NA
    ## 2 VIC              2020-01-26                               NA
    ## 3 NSW              2020-01-27                               NA

</div>

<div id="scan-i" class="section level9">

# Scan I

In this section, the data frame is checked for errors and
inconsistencies. Firstly, consider the logic behind the creation of
`Daily Cases` and `Daily Tests`:

1.  For any given `Province/State`, to recognise days where the values
    of `Cumulative Cases` or `Cumulative Tests` have decreased with
    respect to the day before. These situations represent scenarios
    where data entries have been entered and then corrected at a later
    date. This is easily identified as `Daily Cases` or `Daily Tests`
    will return negative.

2.  For any given `Province/State`, enables an algorithm to be created
    to correct for days where values of `Cumulative Cases` or
    `Cumulative Tests` have decreased with respect to the day before.
    Any negative values and most missing values identified in
    `Daily Cases` and `Daily Tests` will be correctly altered during
    this process.

3.  Afterwards, any further correction of `Daily Cases` and
    `Daily Tests` due to missing values can be performed by imputation.
    This will then enable the `Case to Test Ratio per Million` to be
    easily corrected by running the `mutatecovid()` function.

Note well the above criteria, an algorithm `correctdailycasetest()` is
now made to calculate `Cumulative Cases[i+1] - Cumulative Cases[i]`,
where `i` is a given day. The value of this difference represents the
value of `Daily Cases[i+1]`. For this algorithm to work properly, both
values in `Cumulative Cases` must be numeric, or otherwise
`Daily Cases[i+1]` returns a missing value. The same logic is used to
correct test inconsistencies, so `Cumulative Tests` and `Daily Tests`
have also been incorporated into this function.

``` r
correctdailycasetest <- function(state) {
  x <- covidAU_joined[covidAU_joined$`Province/State` == state, ]
  covidAU_joined[covidAU_joined$`Province/State` == state,][1,5] <<- x$`Cumulative Cases`[1]
  for (i in 1:nrow(covidAU_joined)) {
    if (i < nrow(x)) {
      x$`Daily Cases`[i+1] <- x$`Cumulative Cases`[i+1] - x$`Cumulative Cases`[i]
      covidAU_joined[covidAU_joined$`Province/State` == state,][i+1,5] <<- x$`Daily Cases`[i+1]
      x$`Daily Tests`[i+1] <- x$`Cumulative Tests`[i+1] - x$`Cumulative Tests`[i]
      covidAU_joined[covidAU_joined$`Province/State` == state,][i+1,6] <<- x$`Daily Tests`[i+1]
    }
  }
}
```

The function is applied in date ascending order below. The output has
been filtered to show only those daily cases and tests in `VIC`.
Nevertheless, the same will apply for all those remaining states.

``` r
covidAU_joined <- covidAU_joined %>% arrange(Date)
for (i in c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA')) {
  correctdailycasetest(i)
}
covidAU_joined %>% filter(`Province/State` == 'VIC') %>% head(10) %>% print.data.frame()
```

    ##    Province/State       Date Cumulative Cases Cumulative Tests Daily Cases Daily Tests Case to Test Ratio per Million
    ## 1             VIC 2020-01-26                1               NA           1          NA                             NA
    ## 2             VIC 2020-01-27                1               NA           0          NA                             NA
    ## 3             VIC 2020-01-28                1               NA           0          NA                             NA
    ## 4             VIC 2020-01-29                1               NA           0          NA                             NA
    ## 5             VIC 2020-01-30                2               NA           1          NA                             NA
    ## 6             VIC 2020-01-31                3               NA           1          NA                             NA
    ## 7             VIC 2020-02-01                4               NA           1          NA                             NA
    ## 8             VIC 2020-02-02                4               NA           0          NA                             NA
    ## 9             VIC 2020-02-03                4               NA           0          NA                             NA
    ## 10            VIC 2020-02-04                4               NA           0          NA                             NA

Seen below, the first non-missing values of `Cumulative Tests` and
`Daily Tests` in `covidAU_joined` is `2020-03-08` and `2020-03-09`,
respectively.

``` r
covidAU_joined[which(!is.na(covidAU_joined$`Daily Tests`))[1], ]
```

    ## # A tibble: 1 x 7
    ##   `Province/State` Date       `Cumulative Cas~ `Cumulative Tes~ `Daily Cases`
    ##   <fct>            <date>                <dbl>            <dbl>         <dbl>
    ## 1 NSW              2020-03-09               48             8371            10
    ## # ... with 2 more variables: `Daily Tests` <dbl>, `Case to Test Ratio per
    ## #   Million` <dbl>

For each `Province/State`, the values whereby `Cumulative Cases` or
`Cumulative Tests` have decreased with respect to the day before, are
now identifiable by negative values of `Daily Cases` or `Daily Tests`.

``` r
covidAU_joined %>% filter(`Daily Cases` < 0) %>% count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    19

``` r
covidAU_joined %>% filter(`Daily Tests` < 0) %>% count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1     6

To correct the negative values, consider the following procedure applied
to the case columns of the data frame:

1.  In descending order, calculate `Daily Cases[i] + Daily Cases[i-1]`
    for each given day `i`. This becomes the new value of
    `Daily Cases[i-1]`. For example, if `Daily Cases[i]` is -1 and
    `Daily Cases[i-1]` is 2, so `Daily Cases[i-1]` becomes 1.

2.  In descending order, calculate
    `Daily Cases[i] + Cumulative Cases[i-1]` for each given day `i`.
    This becomes the new value of `Cumulative Cases[i-1]`. For example,
    if `Daily Cases[i]` is -1 and `Cumulative Cases[i-1]` is 100, so
    `Cumulative Cases[i-1]` becomes 99.

3.  The negative value of `Daily Cases[i]` becomes 0. Using the example
    in step one, `Daily Cases[i]` transforms from -1 to 0.

The same logic applies to test columns and there is an equivalent
procedure as above. To implement these procedures, the functions
`correctnegcases()` and `correctnegtests()` have been created, as below.

``` r
correctnegcases <- function(state) {
    x <- covidAU_joined[covidAU_joined$`Province/State` == state, ]
  for (i in nrow(covidAU_joined):1) {
    if (i <= nrow(x)) {
      if (x$`Daily Cases`[i] < 0) {
        x$`Daily Cases`[i-1] <- x$`Daily Cases`[i] + x$`Daily Cases`[i-1]
        covidAU_joined[covidAU_joined$`Province/State` == state,][i-1,5] <<- x$`Daily Cases`[i-1]
        x$`Cumulative Cases`[i-1] <-  x$`Daily Cases`[i] + x$`Cumulative Cases`[i-1]
        covidAU_joined[covidAU_joined$`Province/State` == state,][i-1,3] <<- x$`Cumulative Cases`[i-1]
        covidAU_joined[covidAU_joined$`Province/State` == state,][i,5] <<- 0
      }
    }
  }
}

correctnegtests <- function(state) {
  x <- covidAU_joined[covidAU_joined$`Province/State` == state, ]
  for (i in nrow(covidAU_joined):1) {
    if (i <= nrow(x)) {
      if (x$`Daily Tests`[i] < 0 & !is.na(x$`Daily Tests`[i])) {
        x$`Daily Tests`[i-1] <- x$`Daily Tests`[i] + x$`Daily Tests`[i-1]
        covidAU_joined[covidAU_joined$`Province/State` == state,][i-1,6] <<- x$`Daily Tests`[i-1]
        x$`Cumulative Tests`[i-1] <- x$`Daily Tests`[i] + x$`Cumulative Tests`[i-1]
        covidAU_joined[covidAU_joined$`Province/State` == state,][i-1,4] <<- x$`Cumulative Tests`[i-1]
        covidAU_joined[covidAU_joined$`Province/State` == state,][i,6] <<- 0
      }
    }
  }
}
```

Then running the functions for each `Province/State` then ensures all
negative value corrections have been made.

``` r
for (i in c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA')) {
  correctnegcases(i)
  correctnegtests(i)
}
```

``` r
covidAU_joined %>% filter(`Daily Cases` < 0) %>% count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1     0

``` r
covidAU_joined %>% filter(`Daily Tests` < 0) %>% count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1     0

Many corrections have now been made to `Daily Cases` and `Daily Tests`
by removing the negative values, so running `mutatecovid()` will now
update the values of `Case to Test Ratio per Million`.

``` r
mutatecovid()
covidAU_joined %>% filter(`Province/State` == 'VIC', !is.na(`Daily Tests`)) %>%  
  select(`Province/State`, Date, `Daily Cases`, `Daily Tests`, `Case to Test Ratio per Million`) %>% head(10)
```

    ## # A tibble: 10 x 5
    ##    `Province/State` Date       `Daily Cases` `Daily Tests` `Case to Test Ratio ~
    ##    <fct>            <date>             <dbl>         <dbl>                 <dbl>
    ##  1 VIC              2020-03-17            23          2500                  9200
    ##  2 VIC              2020-03-18            27          1000                 27000
    ##  3 VIC              2020-03-19             0          1980                     0
    ##  4 VIC              2020-03-20             0          2157                     0
    ##  5 VIC              2020-03-21           108          1163                 92863
    ##  6 VIC              2020-03-22           126          2400                 52500
    ##  7 VIC              2020-03-23             0           800                     0
    ##  8 VIC              2020-03-24            56          1300                 43077
    ##  9 VIC              2020-03-25            55           500                110000
    ## 10 VIC              2020-03-26            54          1400                 38571

Running `mutatecovid()` again introduces infinite and NaN values into
the data frame, as a result of divide by zero and zero divide by zero
errors.

``` r
is.infinite(covidAU_joined$`Case to Test Ratio per Million`) %>% sum()
```

    ## [1] 17

``` r
is.nan(covidAU_joined$`Case to Test Ratio per Million`) %>% sum()
```

    ## [1] 34

To fix these errors, a simple function `infNaN()` is created to replace
infinite values with NAs and NaN values with zeros.

``` r
infNaN <- function() {
  for (i in 1:nrow(covidAU_joined)) {
    if (is.infinite(covidAU_joined$`Case to Test Ratio per Million`[i])){
      covidAU_joined$`Case to Test Ratio per Million`[i] <<- NA
    } else if (is.nan(covidAU_joined$`Case to Test Ratio per Million`[i])){
      covidAU_joined$`Case to Test Ratio per Million`[i] <<- 0
    }   
  }
}
infNaN()
```

``` r
is.infinite(covidAU_joined$`Case to Test Ratio per Million`) %>% sum()
```

    ## [1] 0

``` r
is.nan(covidAU_joined$`Case to Test Ratio per Million`) %>% sum()
```

    ## [1] 0

<div id="scan-i" class="section level10">

</div>

# Scan II

In this section, missing values are identified and imputed. The data
frame contains the following missing values for each given variable.

``` r
is.na(covidAU_joined) %>% colSums()
```

    ##                 Province/State                           Date               Cumulative Cases 
    ##                              0                              0                              0 
    ##               Cumulative Tests                    Daily Cases                    Daily Tests 
    ##                            379                              0                            397 
    ## Case to Test Ratio per Million 
    ##                            414

The percentage of missing values in `Cumulative Tests`, `Daily Tests`
and `Case to Test Ratio per Million` for each `Province/State` is as
below.

``` r
for (i in c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA')) {
  paste0(i, ' NAs') %>% print()
  sapply(covidAU_joined[covidAU_joined$`Province/State` == i,c(4,6,7)], function(x) {(sum(is.na(x))/sum(!is.na(x)))*100}) 
    %>% print()
}
```

    ## [1] "ACT NAs"
    ##               Cumulative Tests                    Daily Tests Case to Test Ratio per Million 
    ##                       10.65990                       12.37113                       13.54167 
    ## [1] "NSW NAs"
    ##               Cumulative Tests                    Daily Tests Case to Test Ratio per Million 
    ##                       25.59242                       26.19048                       26.19048 
    ## [1] "NT NAs"
    ##               Cumulative Tests                    Daily Tests Case to Test Ratio per Million 
    ##                       17.36842                       17.98942                       17.98942 
    ## [1] "QLD NAs"
    ##               Cumulative Tests                    Daily Tests Case to Test Ratio per Million 
    ##                       33.67347                       37.17277                       37.17277 
    ## [1] "SA NAs"
    ##               Cumulative Tests                    Daily Tests Case to Test Ratio per Million 
    ##                       33.50515                       34.19689                       34.19689 
    ## [1] "TAS NAs"
    ##               Cumulative Tests                    Daily Tests Case to Test Ratio per Million 
    ##                       21.16402                       24.45652                       25.82418 
    ## [1] "VIC NAs"
    ##               Cumulative Tests                    Daily Tests Case to Test Ratio per Million 
    ##                       30.54187                       31.18812                       33.83838 
    ## [1] "WA NAs"
    ##               Cumulative Tests                    Daily Tests Case to Test Ratio per Million 
    ##                       19.68912                       20.31250                       26.22951

If the 5% rule is applied then removing cases from the data frame will
significantly affect the data distributions. The data frame is instead
corrected by imputation of `Daily Tests`. Both `Cumulative Tests` and
`Case to Test Ratio per Million` are dependent upon `Daily Tests`, so
imputation of `Daily Tests` will allow the other variables to be
corrected by calculations.

Observe the distributions of `Daily Tests` for each given
`Province/State` below.

``` r
par(mfrow = c(3,3), mai=c(0.52,0.52,0.52,0.52))
for (i in c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA')) {
  covidAU_joined[covidAU_joined$`Province/State` == i, 6] %>% 
    unlist() %>% 
    as.vector() %>% 
    hist(x = .,main = paste0('Histogram of Daily Tests in ', i), 
         xlab = 'Number of Daily Tests',
         col = 'dodgerblue')
}
```

<img src="R Code for Preprocessing/missingvaluematrix.svg" width="3000" />

The appropriate imputation method based upon the right-skewness of the
distributions is median.

``` r
for (i in c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA')) {
  covidAU_joined[covidAU_joined$`Province/State` == i, 6] <- covidAU_joined[covidAU_joined$`Province/State` == i, 6] %>% 
    impute(fun = median) %>% 
    round(0)
}
```

See below the first 10 observations in `VIC` and the corresponding
imputed `Daily Tests`.

``` r
covidAU_joined %>% filter(`Province/State` == 'VIC') %>% head(10) %>% print.data.frame()
```

    ##    Province/State       Date Cumulative Cases Cumulative Tests Daily Cases Daily Tests   Case to Test Ratio per Million
    ## 1             VIC 2020-01-26                1               NA           1       13038               NA
    ## 2             VIC 2020-01-27                1               NA           0       13038               NA
    ## 3             VIC 2020-01-28                1               NA           0       13038               NA
    ## 4             VIC 2020-01-29                1               NA           0       13038               NA
    ## 5             VIC 2020-01-30                2               NA           1       13038               NA
    ## 6             VIC 2020-01-31                3               NA           1       13038               NA
    ## 7             VIC 2020-02-01                4               NA           1       13038               NA
    ## 8             VIC 2020-02-02                4               NA           0       13038               NA
    ## 9             VIC 2020-02-03                4               NA           0       13038               NA
    ## 10            VIC 2020-02-04                4               NA           0       13038               NA

The missing values in `Cumulative Tests` are now calculated from
`Daily Tests` by summing these values in ascending order. The function
`correctcumtests()` is created below to carry out this correction.

``` r
correctcumtests <- function(state) {
  x <- covidAU_joined[covidAU_joined$`Province/State` == state, ]
  for (i in 1:nrow(x)) {
    if (i == 1) {
      x$`Cumulative Tests`[i] <- x$`Daily Tests`[i]
      covidAU_joined[covidAU_joined$`Province/State` == state, 4][i,] <<- x$`Cumulative Tests`[i]
    } else {
      x$`Cumulative Tests`[i] <- x$`Cumulative Tests`[i-1] + x$`Daily Tests`[i]
      covidAU_joined[covidAU_joined$`Province/State` == state, 4][i,] <<- x$`Cumulative Tests`[i]  
    }
  }
}
```

The function is applied for each `Province/State` and the corrected
output for `VIC` is shown below.

``` r
for (i in c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA')) {
  correctcumtests(i)
}
```

``` r
covidAU_joined %>% filter(`Province/State` == 'VIC') %>% head(10) %>% print.data.frame()
```

    ##    Province/State       Date Cumulative Cases Cumulative Tests Daily Cases Daily Tests Case to Test Ratio per Million
    ## 1             VIC 2020-01-26                1            13038           1       13038                 NA
    ## 2             VIC 2020-01-27                1            26076           0       13038                 NA
    ## 3             VIC 2020-01-28                1            39114           0       13038                 NA
    ## 4             VIC 2020-01-29                1            52152           0       13038                 NA
    ## 5             VIC 2020-01-30                2            65190           1       13038                 NA
    ## 6             VIC 2020-01-31                3            78228           1       13038                 NA
    ## 7             VIC 2020-02-01                4            91266           1       13038                 NA
    ## 8             VIC 2020-02-02                4           104304           0       13038                 NA
    ## 9             VIC 2020-02-03                4           117342           0       13038                 NA
    ## 10            VIC 2020-02-04                4           130380           0       13038                 NA

Further corrections are now made to `Case to Test Ratio per Million` by
running `mutatecovid()` and `infNaN()`. Shown again is the corrected
output for `VIC` below.

``` r
mutatecovid()
infNaN()
```

``` r
covidAU_joined %>% filter(`Province/State` == 'VIC') %>% head(10) %>% print.data.frame()
```

    ##    Province/State       Date Cumulative Cases Cumulative Tests Daily Cases Daily Tests Case to Test Ratio per Million
    ## 1             VIC 2020-01-26                1            13038           1       13038                 77
    ## 2             VIC 2020-01-27                1            26076           0       13038                 0
    ## 3             VIC 2020-01-28                1            39114           0       13038                 0
    ## 4             VIC 2020-01-29                1            52152           0       13038                 0
    ## 5             VIC 2020-01-30                2            65190           1       13038                 77
    ## 6             VIC 2020-01-31                3            78228           1       13038                 77
    ## 7             VIC 2020-02-01                4            91266           1       13038                 77
    ## 8             VIC 2020-02-02                4           104304           0       13038                 0
    ## 9             VIC 2020-02-03                4           117342           0       13038                 0
    ## 10            VIC 2020-02-04                4           130380           0       13038                 0

The following missing values still remain in the data frame.

``` r
is.na(covidAU_joined) %>% colSums()
```

    ##         Province/State                        Date                  Cumulative Cases            Cumulative Tests 
    ##                      0                           0                                 0                           0 
    ##            Daily Cases                 Daily Tests    Case to Test Ratio per Million                     
    ##                      0                           0                                17

The remaining missing values in `Cases to Test Ratio per Million` result
from a logical inconsistency. Shown below is an observation in `TAS`
where `Daily Tests` are less than `Daily Cases`. A logical constraint
exists demanding that `Daily Tests` are greater than or equal to
`Daily Cases`.

``` r
covidAU_joined[covidAU_joined$`Daily Cases` > 0 & covidAU_joined$`Daily Tests`== 0, ][1,] %>% print.data.frame()
```

    ##   Province/State       Date Cumulative Cases Cumulative Tests Daily Cases Daily Tests Case to Test Ratio per Million
    ## 1            TAS 2020-03-28               62            13433          15           0                             NA

</div>

</div>
