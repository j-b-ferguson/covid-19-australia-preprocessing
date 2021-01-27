<h1><p align="center">COVID-19 Australian Data Cleaning</h1><a href="https://www.justinferguson.me/pages/COVID-19_Aus_cleaned.html"><span style="font-size: 6px;">Report Version</span></a>

<p align="center"><b>Author</b></p>
<a href="https://github.com/j-b-ferguson"><p align="center">Justin Ferguson GitHub</p></a>
<a href="https://www.linkedin.com/in/j-b-ferguson/"><p align="center">Justin Ferguson LinkedIn</p></a>
<a href="mailto:justin.benjamin.ferguson@gmail.com?subject=GitHub%20Enquiry"><p align="center">Contact</p></a>

<h2><p align=center>Executive Summary</h2>

This notebook brings together data of worldwide COVID-19 cases and Australian COVID-19 testing. Through a series of pre-processing steps, the data is filtered for cases and tests relevant to Australian states. It is then tidied, scanned, and transformed, as required.

To begin, the data sets are imported and saved. All variables and their data types are inspected and their context understood. Following the tidy data principles, the data frames are put into tidy format by reshaping the data frames from wide to long format. Afterwards, proper data type conversions are performed to ensure all types are logically correct. Now in tidy format, with type conversions completed, the data frames are joined by their common keys with a left join. Then manipulating, the data set is filtered row-wise and two new variables are inserted.

The following sections identify and rectify a number of errors, inconsistencies, and missing values present in the joined data set. Such as negative day-to-day case or test values, or days where cases are greater than performed tests. Through a series of user-defined functions and machine learning imputation methods, the errors, inconsistencies, and missing values are corrected. Visualisations of the time-series is then used to locate and reduce noise with a k-nearest neighbours algorithm. This improves the overall smoothness of the data set for later analysis and predictive modelling.

Finally, seven and fourteen day moving averages are added to complement the short term daily movement in COVID-19 cases and tests. These long term movements should allow for greater insights in data analysis and perhaps improve predictive modelling.

<h2><p align=center>Packages</h2>

```r
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

<h2><p align=center>Data Summary</h2>

The first data set is a time-series obtained from the COVID-19 GitHub repository of John Hopkins Whiting School of Engineering, Centre for Systems Science and Engineering. The data set contains the daily cumulative counts of COVID-19 cases in various countries and regions, as well as applicable provinces and states. The data set was saved into CSV format by right-clicking the ‘Raw’ icon inside the link below.

[John Hopkins COVID-19 Cases Data](https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv)

| Variable | Name               | Description                                                                                                                                   |
|----------|--------------------|-----------------------------------------------------------------------------------------------------------------------------------------------|
| 1        | Province/State     | Provinces, states and dependencies of the following countries: Australia, Canada, China, Netherlands, United Kingdom, France and Denmark      |
| 2        | Country/Region     | Countries and regions with COVID-19 cases                                                                                                     |
| 3        | Lat                | Latitude in degrees of Country/Region or corresponding Province/State (if applicable)                                                         |
| 4        | Long               | Longtitude in degrees of Country/Region or corresponding Province/State (if applicable)                                                       |
| 5 - 273  | 1/22/20 … 10/16/20 | Cumulative counts of daily COVID-19 cases in their corresponding Country/Region or Province/State (if applicable) from 1/22/20 until 10/16/20 |

```r
cases <- read_csv('~/COVID-19-AUSTRALIA-PREPROCESSING/Original Untidy Data Sets/time_series_covid19_confirmed_global.csv')
head(cases, 3) # Show first 3 observations of cases data frame
```

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
```

The second data set is a time-series made available by www.covid19data.com.au. The data set gives the daily cumulative counts of COVID-19 tests in Australian provinces and states. The CSV data set was extracted from the below link by clicking the ‘download data’ icon.

[Infogram COVID-19 Tests Data](https://e.infogram.com/_/3osqzRmYBiJsJafg79YC?parent_url=https%3A%2F%2Fwww-covid19data-com-au.filesusr.com%2Fhtml%2F2aed08_944ecbfd558f24812177bca5a8a74000.html&src=embed#)

| Variable | Name                                | Description                                                                                                   |
|----------|-------------------------------------|---------------------------------------------------------------------------------------------------------------|
| 1        | (left as blank)                     | Days in 2020 of format dd-month from 8-Mar until 18-Oct                                                       |
| 2 - 8    | NSW, VIC, QLD, SA, WA, TAS, NT, ACT | Cumulative counts of daily COVID-19 tests in NSW, VIC, QLD, SA, WA, TAS, NT, ACT (Australia) for a given date |


```r
tests <- read_csv('~/COVID-19-AUSTRALIA-PREPROCESSING/Original Untidy Data Sets/Total tests.csv')
head(tests, 3) # Show first 3 observations of tests data frame
```

```
## # A tibble: 3 x 9
##   X1      NSW   VIC   QLD    SA    WA   TAS    NT   ACT
##   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 08/03  8008    NA    NA    NA    NA    NA    NA    NA
## 2 09/03  8371    NA    NA    NA    NA    NA    NA    NA
## 3 10/03 10165    NA    NA    NA    NA    NA    NA    NA
```

In their current formats, there are no common variables for these data sets to be joined by. The data sets must first be reshaped into tidy format. This process is covered later.

<h2><p align=center>Understand Data Frames</h2>

Before both data sets are altered, the data structure must first be understood. The class of both data frames are shown below as data frames.

```r
list(class(cases), class(tests)) # Show data structure
```

```
## [[1]]
## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame" 
## 
## [[2]]
## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"
```

The output below gives the dimensions of the data frames as observations (rows) and variables (columns), respectively.

```r
list(dim(cases), dim(tests)) # Show data frame dimensions
```

```
## [[1]]
## [1] 267 273
## 
## [[2]]
## [1] 211   9
```

The overall structure of the data is now examined. This is normally achieved with the `str()` function, which allows each variable and its corresponding data type to be checked. However, with the `cases` data frame, there are 273 variables, so a few short cuts are taken to reduce the output. Using the `sapply()` function, the data type of all columns are inspected. A short cut to inspect the date variables is to check for unique data types.

```r
# Show data types in cases data frame
list(sapply(cases[,1:4], class),
     sapply(cases[,5:273], class) %>% unique() %>% paste('Variables 1/22/20 ... 10/16/20 are all', .))
```

```
## [[1]]
## Province/State Country/Region            Lat           Long 
##    "character"    "character"      "numeric"      "numeric" 
## 
## [[2]]
## [1] "Variables 1/22/20 ... 10/16/20 are all numeric"
```

The corresponding structure of the `tests` data frame is given in the output below.

```r
# Show data types in tests data frame
str(tests, give.attr = FALSE)
```

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
```

The next step is usually to apply the proper data type conversions. However, it is more convenient in this case to reshape both data frames into long format first.

<h2><p align=center>Reshape Data Frames</h2>

In this section, the tidy data principles are applied to both data frames to enable further pre-processing. For reference, recall the tidy data principles:

Condition 1 - Each variable is its own column,

Condition 2 - Each observation is its own row,

Condition 3 - Each value is its own cell.

Starting with the `cases` data frame, the dates that form the column names from columns 5 to 273 are defined as variables, when in fact they are values and should be in cells of their own under a new variable name, such as 'Date'. The values assigned to the date variables should also be separate from the dates themselves, in a new variable such as 'Cumulative Cases'. Hence, the `cases` data frame is not in a tidy format, and will require reshaping from wide to long format with `gather()`.

Using `gather()`, the dates that currently form the variables will instead be values under a new variable named `Date`. The values that currently exist under each date variable will instead form values under a new variable named `Cumulative Cases`.

```r
# Reshape cases data frame from wide to long format
cases_gathered <- gather(cases, key = 'Date', value = 'Cumulative Cases', 5:273)
head(cases_gathered, 3)
```

```
## # A tibble: 3 x 6
##   `Province/State` `Country/Region`   Lat  Long Date    `Cumulative Cases`
##   <chr>            <chr>            <dbl> <dbl> <chr>                <dbl>
## 1 <NA>             Afghanistan       33.9 67.7  1/22/20                  0
## 2 <NA>             Albania           41.2 20.2  1/22/20                  0
## 3 <NA>             Algeria           28.0  1.66 1/22/20                  0
```

Inspect now the `tests` data frame, the state names that form the column names from columns 2 to 9 are defined as variables, when in fact they are values, and should be put into their own cells under a new variable name, such as 'Province/State'. The values assigned to the state variables should also be separate from the states themselves, under a new variable such as 'Cumulative Tests'. Hence, the `tests` data frame is in an untidy format, and will also required reshaping from wide to long format.

Applying `gather()`, the states that currently form the variables will instead be values under a new variable named `Province/State`. The values that currently exist under each state variable will instead form values under a new variable named `Cumulative Tests`. Note: `Province/State` is now a variable common to both data frames.

```r
# Reshape tests data frame from wide to long format
tests_gathered <- tests %>% gather(key = 'Province/State', value = 'Cumulative Tests', 2:9)
head(tests_gathered, 3)
```

```
## # A tibble: 3 x 3
##   X1    `Province/State` `Cumulative Tests`
##   <chr> <chr>                         <dbl>
## 1 08/03 NSW                            8008
## 2 09/03 NSW                            8371
## 3 10/03 NSW                           10165
```

<h2><p align=center>Convert Data Types</h2>

Now both data frames are in tidy format, proper data type conversions can be performed. Beginning with `tests_gathered`, the column name of the first variable is changed from `X1` to `Date`. This variable is common to both data frames.

```r
# Rename first variable of tests_gathered data frame
colnames(tests_gathered)[1] <- 'Date'
names(tests_gathered)
```

```
## [1] "Date"             "Province/State"   "Cumulative Tests"
```

The format of `Date` is changed into DD/MM/YY and a type conversion from character to date is performed.

```r
# Change date format to DD/MM/YY format and convert from character to date type in tests_gathered data frame
tests_gathered$Date <- replace(tests_gathered$Date, values = paste0(tests_gathered$Date, '/20'))
tests_gathered$Date <- tests_gathered$Date %>% as.Date(., "%d/%m/%y")
```

Next, since the cumulative test numbers evolve with respect to their isolated states, it is logical to convert the `Province/State` variable from character type to factor.

```r
# Convert Province/State to factor type in tests_gathered data frame
tests_gathered$`Province/State` <- tests_gathered$`Province/State` %>% as.factor()
```

The converted `tests_gathered` data frame now has the following structure.

```r
# Check data structure of tests_gathered data frame
str(tests_gathered)
```

```
## tibble [1,688 x 3] (S3: tbl_df/tbl/data.frame)
##  $ Date            : Date[1:1688], format: "2020-03-08" "2020-03-09" ...
##  $ Province/State  : Factor w/ 8 levels "ACT","NSW","NT",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ Cumulative Tests: num [1:1688] 8008 8371 10165 10221 14856 ...
```

Now consider `cases_gathered`, a subset of this data frame is taken with observations of interest corresponding to Australian cases.

```r
# Subset cases_gathered data frame for observations concerning Australia
cases_gathered <- cases_gathered %>% filter(`Country/Region` == 'Australia') %>% 
  select(`Province/State`, Date, `Cumulative Cases`)
# Check first three observations of subset cases_gathered data frame 
head(cases_gathered, 3)
```

```
## # A tibble: 3 x 3
##   `Province/State`             Date    `Cumulative Cases`
##   <chr>                        <chr>                <dbl>
## 1 Australian Capital Territory 1/22/20                  0
## 2 New South Wales              1/22/20                  0
## 3 Northern Territory           1/22/20                  0
```

The `Province/State` variable is then converted from character type to factor, and the values are abbreviated to match those stored in `tests_gathered`.

```r
# Convert Province/State to factor type in cases_gathered data frame and rename variable labels
cases_gathered$`Province/State` <- cases_gathered$`Province/State`%>% as.factor() %>% 
  factor(levels = c('Australian Capital Territory', 'New South Wales', 'Northern Territory', 'Queensland', 'South Australia', 'Tasmania', 'Victoria', 'Western Australia'),
         labels = c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA'))
```

Data type conversion is then performed on the correctly formatted `date` variable.

```r
# Convert from character to date type in cases_gathered data frame
cases_gathered$Date <- cases_gathered$Date %>% as.Date(., "%m/%d/%y")
```

The converted `cases_gathered` data frame now has the following structure.

```r
# Check data structure of cases_gathered data frame
str(cases_gathered)
```

```
## tibble [2,152 x 3] (S3: tbl_df/tbl/data.frame)
##  $ Province/State  : Factor w/ 8 levels "ACT","NSW","NT",..: 1 2 3 4 5 6 7 8 1 2 ...
##  $ Date            : Date[1:2152], format: "2020-01-22" "2020-01-22" ...
##  $ Cumulative Cases: num [1:2152] 0 0 0 0 0 0 0 0 0 0 ...
```

<h2><p align=center>Join Data Frames</h2>

A left join is now carried out to merge the data frames by the common variables `Date` and `Province/State`.

```r
# Left join tests_gathered onto cases_gathered by both data sets by "Province/State" and "Date"  
covidAU_joined <- left_join(cases_gathered, tests_gathered, 
                             by = c("Province/State", "Date"))
```

The joined data frame is then filtered from the date of the first recorded case `2020-01-26`.

```r
# Filter cumulative cases greater than zero
covidAU_joined <- covidAU_joined[covidAU_joined$`Cumulative Cases` > 0,]
# Preview new data set
head(covidAU_joined)
```

```
## # A tibble: 6 x 4
##   `Province/State` Date       `Cumulative Cases` `Cumulative Tests`
##   <fct>            <date>                  <dbl>              <dbl>
## 1 NSW              2020-01-26                  3                 NA
## 2 VIC              2020-01-26                  1                 NA
## 3 NSW              2020-01-27                  4                 NA
## 4 VIC              2020-01-27                  1                 NA
## 5 NSW              2020-01-28                  4                 NA
## 6 VIC              2020-01-28                  1                 NA
```

<h2><p align=center>Create Variables</h2>

Two variables `Daily Cases` and `Daily Tests` are now created from values of `Cumulative Cases` and `Cumulative Tests` by calculation. These new variables allow errors and inconsistencies to be identified in the `Cumulative Cases` and `Cumulative Tests` columns.

```r
# Insert new variables as NAs
covidAU_joined$`Daily Cases` <- as.numeric(NA)
covidAU_joined$`Daily Tests` <- as.numeric(NA)
covidAU_joined %>% select(`Province/State`, Date, `Daily Cases`, `Daily Tests`) %>% head(3)
```

```
## # A tibble: 3 x 4
##   `Province/State` Date       `Daily Cases` `Daily Tests`
##   <fct>            <date>             <dbl>         <dbl>
## 1 NSW              2020-01-26            NA            NA
## 2 VIC              2020-01-26            NA            NA
## 3 NSW              2020-01-27            NA            NA
```

These new variables aid in the following:

1.  For any given `Province/State`, days where the values
    of `Cumulative Cases` or `Cumulative Tests` have decreased with
    respect to the day before are well recognised. These situations represent scenarios
    where data entries have been entered and then corrected at a later
    date. Values of `Daily Cases` or `Daily Tests` will return negative in such cases.

2.  For any given `Province/State`, enables an algorithm to be created
    to correct for days where values of `Cumulative Cases` or
    `Cumulative Tests` have decreased with respect to the day before.
    Any negative values and most of the missing values identified in
    `Daily Cases` and `Daily Tests` will be correctly altered during
    this process.
    
Note well the above criteria, an algorithm `correctdailycasetest()` is now made to calculate `Cumulative Cases[i+1] - Cumulative Cases[i]`, where `i` is a given day. The value of this difference represents the value of `Daily Cases[i+1]`. For this algorithm to work properly, both values in `Cumulative Cases` must be numeric, or otherwise `Daily Cases[i+1]` returns a missing value. The same logic is used to correct test inconsistencies, so `Cumulative Tests` and `Daily Tests` have also been incorporated into this function.

```r
# A function to calculate daily case and daily test values from cumulative case and cumulative test values
correctdailycasetest <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_joined[covidAU_joined$`Province/State` == state, ]
  # For each state, save the first cumulative case as the first daily case
  covidAU_joined[covidAU_joined$`Province/State` == state,][1, 'Daily Cases'] <<- x$`Cumulative Cases`[1]
  # A forward loop to calculate daily case and daily test values
  for (i in seq(1, nrow(x)-1)) {
      x$`Daily Cases`[i+1] <- x$`Cumulative Cases`[i+1] - x$`Cumulative Cases`[i]
      covidAU_joined[covidAU_joined$`Province/State` == state,][i+1, 'Daily Cases'] <<- x$`Daily Cases`[i+1]
      x$`Daily Tests`[i+1] <- x$`Cumulative Tests`[i+1] - x$`Cumulative Tests`[i]
      covidAU_joined[covidAU_joined$`Province/State` == state,][i+1, 'Daily Tests'] <<- x$`Daily Tests`[i+1]
  }
}
```

The function is then applied over each state.


```r
# Execute correctdailycasetest function over each state
states <- c('ACT', 'NT', 'NSW', 'QLD', 'SA', 'TAS', 'VIC', 'WA')
for (i in states) {
  correctdailycasetest(i)
}
```

The output below has been filtered to show only those daily cases and tests in `VIC`. Nevertheless, the correction has been applied to all states.

```r
# Filter and show first 10 observations for Victoria
covidAU_joined %>% filter(`Province/State` == 'VIC') %>% head(10)
```

```
## # A tibble: 10 x 6
##    `Province/State` Date       `Cumulative Cas~ `Cumulative Tes~ `Daily Cases`
##    <fct>            <date>                <dbl>            <dbl>         <dbl>
##  1 VIC              2020-01-26                1               NA             1
##  2 VIC              2020-01-27                1               NA             0
##  3 VIC              2020-01-28                1               NA             0
##  4 VIC              2020-01-29                1               NA             0
##  5 VIC              2020-01-30                2               NA             1
##  6 VIC              2020-01-31                3               NA             1
##  7 VIC              2020-02-01                4               NA             1
##  8 VIC              2020-02-02                4               NA             0
##  9 VIC              2020-02-03                4               NA             0
## 10 VIC              2020-02-04                4               NA             0
## # ... with 1 more variable: `Daily Tests` <dbl>
```

<h2><p align=center>Correct Negative Values</h2>

Negative values of daily cases or tests represent data entry corrections and should be corrected to avoid errors in later stages of pre-processing. The following chunk counts the number of observations with cases or tests less than zero.

```r
# Count number of observations with cases or tests less than zero
covidAU_joined %>% filter(`Daily Cases` < 0) %>% count()
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1    19
```

```r
covidAU_joined %>% filter(`Daily Tests` < 0) %>% count()
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1     6
```

To correct the negative values, consider the following procedure applied to the case variable of the data frame:

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

The same logic applies to the test variable and there is an equivalent procedure as above. To implement these procedures, the functions `correctnegcases()` and `correctnegtests()` have been created, as below.

```r
# A function to correct negative daily cases and decreasing cumulative cases
correctnegcases <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_joined[covidAU_joined$`Province/State` == state, ]
  # A reverse loop
  for (i in nrow(x):1) {
    # Condition to apply the function to negative values only
    if (x$`Daily Cases`[i] < 0) {
      x$`Daily Cases`[i-1] <- x$`Daily Cases`[i] + x$`Daily Cases`[i-1]
      covidAU_joined[covidAU_joined$`Province/State` == state,][i-1, 'Daily Cases'] <<- x$`Daily Cases`[i-1]
      x$`Cumulative Cases`[i-1] <-  x$`Daily Cases`[i] + x$`Cumulative Cases`[i-1]
      covidAU_joined[covidAU_joined$`Province/State` == state,][i-1, 'Cumulative Cases'] <<- x$`Cumulative Cases`[i-1]
      covidAU_joined[covidAU_joined$`Province/State` == state,][i, 'Daily Cases'] <<- 0
    } else {
      invisible()
    }
  }
}
```

```r
# A function to correct negative daily tests and decreasing cumulative tests
correctnegtests <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_joined[covidAU_joined$`Province/State` == state, ]
  # A reverse loop
  for (i in nrow(x):1) {
    # Condition to apply the function to negative values only
    if (x$`Daily Tests`[i] < 0 & !is.na(x$`Daily Tests`[i])) {
      x$`Daily Tests`[i-1] <- x$`Daily Tests`[i] + x$`Daily Tests`[i-1]
      covidAU_joined[covidAU_joined$`Province/State` == state,][i-1, 'Daily Tests'] <<- x$`Daily Tests`[i-1]
      x$`Cumulative Tests`[i-1] <- x$`Daily Tests`[i] + x$`Cumulative Tests`[i-1]
      covidAU_joined[covidAU_joined$`Province/State` == state,][i-1, 'Cumulative Tests'] <<- x$`Cumulative Tests`[i-1]
      covidAU_joined[covidAU_joined$`Province/State` == state,][i, 'Daily Tests'] <<- 0
    } else {
      invisible()
    }
  }
}
```

Then running the functions for each state ensures all negative value corrections have been made.

```r
# Execute correctnegcases and correctnegtests functions over each state
for (i in states) {
  correctnegcases(i)
  correctnegtests(i)
}
```

The below output shows that no negative values remain.

```r
covidAU_joined %>% filter(`Daily Cases` < 0) %>% count()
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1     0
```

```r
covidAU_joined %>% filter(`Daily Tests` < 0) %>% count()
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1     0
```

<h2><p align=center>Logical Corrections and Pruning</h2>

The output below shows observations where `Daily Cases` are greater than `Daily Tests`. If an assumption is made that requires all cases to be linked to tests made on the same day, then this is logically incorrect because tests should always be greater than or equal to diagnosed cases.

```r
# Count observations where daily cases are greater than daily tests - logically incorrect
covidAU_joined %>% filter(`Daily Cases` > `Daily Tests`) %>% select(`Daily Tests`) %>% count()
```

```
## # A tibble: 1 x 1
##       n
##   <int>
## 1    17
```

Therefore, observations where `Daily Cases` are greater than `Daily Tests` are assumed erroneous and transformed into missing values.

```r
# Observations where daily cases are greater than daily tests are assumed erroneous and made missing values
covidAU_joined[!is.na(covidAU_joined$`Daily Tests`) & covidAU_joined$`Daily Tests` < covidAU_joined$`Daily Cases`, 'Daily Tests'] <- NA
```

There are a number of missing values contained in the data frame before the first complete observation for each state. These missing values shall be removed from the data set to avoid accidentally introducing bias caused by imputation methods.

```r
#Arrange data frame by state
covidAU_joined <- covidAU_joined %>% arrange(`Province/State`)

# For each state, remove observations where daily test values are incomplete prior to first complete case
for (i in states) {
  # Index of first missing value in daily test variable for each state prior to first complete case
  start <- which(covidAU_joined$`Province/State` == i & is.na(covidAU_joined$`Daily Tests`))[1]
  # Index of last missing value in daily test variable for each state prior to first complete case
  finish <- which(covidAU_joined$`Province/State` == i & !is.na(covidAU_joined$`Daily Tests`))[1] - 1
  # Remove missing values in daily test variable prior to first complete case for each province/state
  covidAU_joined <- covidAU_joined[-seq(start, finish), ]
}
```

Further, there are missing values at the end of the data frame for each state that must be removed to avoid bias, beginning at `2020-10-05` and ending at `2020-10-16`.

```r
# Arrange by date
covidAU_joined <- covidAU_joined %>% arrange(Date)

# Find start date first instance
start <- which(covidAU_joined$Date == '2020-10-05')[1]

# Find end date last instance
finish <- which(covidAU_joined$Date == '2020-10-16')[8]

# Daily tests for dates at the end of data frame from 2020-10-05 to 2020-10-16 are missing for all states - remove these missing cases
covidAU_joined <- covidAU_joined[-seq(start,finish),]
```

The data frame has now effectively been pruned at both ends. To conclude this section, assume that any remaining zeros contained within the `Daily Tests` variable are erroneous due to the extensive amount of testing performed throughout this period of data collection.  

```r
# Assume that any remaining zeros in daily tests are incorrect. Transform to missing values and impute later
covidAU_joined[covidAU_joined$`Daily Tests` == 0 & !is.na(covidAU_joined$`Daily Tests`), 'Daily Tests'] <- NA
```

<h2><p align=center>Check Missing Values and Plot Distributions</h2>

The data frame has the following total missing values in each column.

```r
# Count of missing values in each variable independent of province/state
is.na(covidAU_joined) %>% colSums()
```

```
##   Province/State             Date Cumulative Cases Cumulative Tests      Daily Cases      Daily Tests 
##                0                0                0               12                0               71
```

The percentage of missing values in each variable for each state is given below.

```r
# Percentage of missing values in each variable for each province/state
for (i in states) {
  statei <- covidAU_joined[covidAU_joined$`Province/State` == i,]
  paste(i, 'missing values') %>% print()
  sapply(statei, function(x) {(sum(is.na(x)) / sum(!is.na(x))) * 100}) %>% print()
}
```

```
## [1] "ACT missing values"
##   Province/State             Date Cumulative Cases Cumulative Tests      Daily Cases      Daily Tests 
##        0.0000000        0.0000000        0.0000000        0.5128205        0.0000000        3.1578947 
## [1] "NT missing values"
##   Province/State             Date Cumulative Cases Cumulative Tests      Daily Cases      Daily Tests 
##         0.000000         0.000000         0.000000         0.000000         0.000000         2.162162 
## [1] "NSW missing values"
##   Province/State             Date Cumulative Cases Cumulative Tests      Daily Cases      Daily Tests 
##                0                0                0                0                0                0 
## [1] "QLD missing values"
##   Province/State             Date Cumulative Cases Cumulative Tests      Daily Cases      Daily Tests 
##         0.000000         0.000000         0.000000         2.051282         0.000000         4.188482 
## [1] "SA missing values"
##   Province/State             Date Cumulative Cases Cumulative Tests      Daily Cases      Daily Tests 
##          0.00000          0.00000          0.00000          0.00000          0.00000          1.04712 
## [1] "TAS missing values"
##   Province/State             Date Cumulative Cases Cumulative Tests      Daily Cases      Daily Tests 
##         0.000000         0.000000         0.000000         3.743316         0.000000         7.777778 
## [1] "VIC missing values"
##   Province/State             Date Cumulative Cases Cumulative Tests      Daily Cases      Daily Tests 
##         0.000000         0.000000         0.000000         0.000000         0.000000         2.538071 
## [1] "WA missing values"
##   Province/State             Date Cumulative Cases Cumulative Tests      Daily Cases      Daily Tests 
##                0                0                0                0                0               20
```

Counts of missing values in each variable for each state are well visualised with the missing value matrix below. On the right-hand side of each matrix are the number of variables with missing values. On the left-hand side are the number of observations corresponding to those on the right-hand side. Below each matrix are the number of missing values in each variable.

```r
svglite('~/COVID-19-AUSTRALIA-PREPROCESSING/R Code for Preprocessing/missingvaluematrix.svg')
par(mar = c(0,0,0,0))
layout.matrix <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol =3)
layout(mat = layout.matrix)
MVStatematrix <- for (i in states) {
  MVState <- covidAU_joined[covidAU_joined$`Province/State` == i,]
  md.pattern(MVState, rotate.names = TRUE)
  mtext(i, 2, las=2, line = -4)
}
dev.off()
```

<img src="R Code for Preprocessing/missingvaluematrix.svg" width="3000" />

Missing values are shown in either `Cumulative Tests` or `Daily Tests`, or both. To preserve the continuity of the time-series data frame, missing values shall not be removed but imputed instead. To assist imputation methods, the distribution of tests are plotted below for each state. N.B. `Cumulative Tests` are corrected after imputation of `Daily Tests`, so there is no need to visualise the distributions of this variable. 


```r
# Plot histograms, frequency polygon and box plots of COVID-19 daily tests to observe distributions and skewness
distsvg <- ggplot(covidAU_joined, aes(`Daily Tests`)) +
           geom_histogram(aes(fill = ..x..), show.legend = FALSE, bins = 10) +
           geom_freqpoly(size = 0.5, bins = 10, color = '#50C878') +
           scale_color_manual(values=c("#CC6666", "#9999CC")) +
           stat_boxplot(geom ='errorbar', position = position_nudge(y = -20), width = 20) +
           geom_boxplot(position = position_nudge(y = -20), width = 20) +
           facet_wrap(vars(`Province/State`), scales = "free_x") +
           ylab("Count") +
           ggtitle("Distributions of Daily COVID-19 Tests for each State") +
           theme(plot.title = element_text(hjust = 0.5, size = 16, vjust = 1, face = "bold"),
              plot.background = element_rect(fill = "#FFFFFF"),
              panel.background = element_rect(fill = "#FFFFFF", colour = "#ededed", size = 2, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#ededed"), 
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
              axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
ggsave(file="~/COVID-19-AUSTRALIA-PREPROCESSING/R Code for Preprocessing/dailytestdistributions.svg", plot=distsvg, width=10, height=8, dpi = 300)
```

<img src="R Code for Preprocessing/dailytestdistributions.svg" width="3000" />

<h2><p align=center>Finding the Optimum Regression Statistic for Imputation</h2>

In this section, the optimum regression statistics for imputing missing values in `Daily Tests` are evaluated. The regression statistics used are either median, mean or k-nearest neighbours. Firstly, note `NSW` has no missing values, so imputation is not necessary here. 


```r
# NSW has no missing values, so redefine states variable as below
states <- c("ACT", "NT", "QLD", "SA", "TAS", "VIC", "WA")
```

A machine learning test set is then created from a subset of the data frame for each state. The test set extracts only complete cases from the data frame.

```r
# Subset the data frame for a machine learning test set, where each province/state has missing values removed from the daily test variable
for (i in states) {
  assign(paste0('testset_', i), covidAU_joined[!is.na(covidAU_joined$`Daily Tests`) & covidAU_joined$`Province/State` == i,])
}
```

The missing value count from the `covidAU_joined` data frame for each state are saved as new variables. This is done to assist in the creation of a machine learning training set. The count of missing values in the training set for each state will be proportional to those in the original data frame.

```r
# Calculate the missing value count for each province/state from the covidAU_joined data frame and save as a variable
for (i in states) {
  statetesti <- covidAU_joined[covidAU_joined$`Province/State` == i & is.na(covidAU_joined$`Daily Tests`), 'Daily Tests']
  mvcounti <- count(statetesti)
  assign(paste0('missingvalues_', i), mvcounti$n)
}
```

A machine learning training set is created by injecting artificial missing values into a duplicated test set.

```r
# Create machine learning training set by injecting a random sample of missing values into the test set
set.seed(1234)
for (i in states) {
  # Get random sample indexes corresponding to the count of missing values for each province/state
  sampleindex <- sample(1:nrow(eval(as.name(paste0('testset_', i)))), eval(as.name(paste0('missingvalues_', i))))
  # Get observations from test set corresponding to the random sample indexes
  sampleobs <- eval(as.name(paste0('testset_', i)))[sampleindex, "Daily Tests"]
  # Duplicate test set as training set
  trainingset <- eval(as.name(paste0('testset_', i)))
  # Value match random sample observations with respect to the training set and inject missing values
  trainingset[trainingset$`Daily Tests` %in% sampleobs$`Daily Tests`, "Daily Tests"] <- NA
  # Save training sets with artificial missing values injected
  assign(paste0('trainingset_', i), trainingset)
}
```

Now compare the counts of missing values between the training set and the original data frame.

```r
# Show original count of missing values in covidAU_joined data frame for each province/state
for (i in states) {
  covidAU_joined[covidAU_joined$`Province/State` == i, ]%>% is.na() %>% sum() %>% print()
}
```

```
## [1] 7
## [1] 4
## [1] 12
## [1] 2
## [1] 21
## [1] 5
## [1] 32
```

```r
# Compare with training data the count of missing values for each province/state
for (i in states) {
  eval(as.name(paste0('trainingset_', i))) %>% is.na() %>% sum() %>% print()
}
```

```
## [1] 6
## [1] 5
## [1] 8
## [1] 2
## [1] 18
## [1] 5
## [1] 33
```

The following code chunk evaluates the optimum regression statistics for imputation by comparing loss functions between the test and training sets. The loss functions are the Mean Absolute Error, Mean Square Error, Root Mean Square Error, and Mean Absolute Percentage Error. The regression statistic with the least error is the preferred method of imputation.

```r
for (i in states) {
  # Temporary placeholder variables for use in loop
  testset <- eval(as.name(paste0('testset_', i)))
  trainingset <- eval(as.name(paste0('trainingset_', i)))
  
  # Get values of daily test variable from test set which correspond to artificial missing values injected into training set
  assign(paste0('testsetactualvalues_', i), testset[is.na(trainingset$`Daily Tests`), "Daily Tests"])
  testsetactualvalues <- eval(as.name(paste0('testsetactualvalues_', i)))   
  
  # Get predicted median regression statistics
  assign(paste0('predictedMedian_', i), rep(median(testsetactualvalues$`Daily Tests`), length(testsetactualvalues$`Daily Tests`)))
  predictedMedian <- eval(as.name(paste0('predictedMedian_', i)))
  
  # Get predicted mean regression statistics
  assign(paste0('predictedMean_', i), rep(mean(testsetactualvalues$`Daily Tests`), length(testsetactualvalues$`Daily Tests`)))
  predictedMean <- eval(as.name(paste0('predictedMean_', i)))
  
  # Perform k-nearest neighbours imputation on training set and get predicted kNN regression statistics
  k <- nrow(trainingset) %>% sqrt() %>% floor()
  assign(paste0('kNN_', i), kNN(trainingset, variable = "Daily Tests", k))
  kNNdata <- eval(as.name(paste0('kNN_', i)))
  assign(paste0('predictedkNN_', i), kNNdata[is.na(trainingset$`Daily Tests`), "Daily Tests"])
  predictedkNN <- eval(as.name(paste0('predictedkNN_', i)))
  
  # Calculate error from loss functions for each regression statistic
  assign(paste0('lossMedian_', i), regr.eval(testsetactualvalues, predictedMedian))
  assign(paste0('lossMean_', i), regr.eval(testsetactualvalues, predictedMean))
  assign(paste0('losskNN_', i), regr.eval(testsetactualvalues, predictedkNN))
  
  # Temporary placeholder variables
  lossMedian <- eval(as.name(paste0('lossMedian_', i)))
  lossMean <- eval(as.name(paste0('lossMean_', i)))
  losskNN <- eval(as.name(paste0('losskNN_', i)))
  
  # Compare error between loss functions for each regression statistic and evaluate preferred regression statistic for imputation
  lossCompare1 <- data.frame(x1 = ifelse(lossMedian < losskNN & lossMedian < lossMean, 'Median', ''))
  lossCompare2 <- data.frame(x2 = ifelse(lossMean < losskNN & lossMean < lossMedian, 'Mean', ''))
  lossCompare3 <- data.frame(x3 = ifelse(losskNN < lossMean & losskNN < lossMedian, 'kNN', ''))
  lossCompare4 <- data.frame(x4 = ifelse(lossMedian == lossMean & lossMean < losskNN, 'Median/Mean', ''))
  
  # Bind together loss functions with preferred regression statistic for each province/state
  lossCompare <- cbind(lossCompare1, lossCompare2, lossCompare3, lossCompare4)
  lossCompare <- lossCompare %>% unite('x1', 1:4, sep = '')
  lossCompare[lossCompare$x1 == '',] <- NA; colnames(lossCompare) <- i
  assign(paste0('loss', i), data.frame(lossCompare, row.names = toupper(names(lossMedian))))
}

# Create a matrix to easily evaluate the preferred regression statistic given the loss functions
lossmatrix <- bind_cols(lossACT, lossNT, lossQLD, lossSA, lossTAS, lossVIC, lossWA) %>% as.matrix()

# Plot loss matrix
svglite('~/COVID-19-AUSTRALIA-PREPROCESSING/R Code for Preprocessing/lossmatrix.svg')
par(mfrow = c(1,1), mai = c(1,1.3,1,1.3))
plot(lossmatrix, 
     col = c(rev(brewer.pal(4, 'Blues'))), 
     xlab = 'State', 
     ylab = 'Loss Functions',
     main = 'Preferred Regression Statistic for Imputing Missing \nValues given Loss Function')
dev.off()
```

<img src="R Code for Preprocessing/lossmatrix.svg" width="3000" />

<h2><p align=center>Impute Missing Values</h2>

Consider the missing value matrix plot above. Based upon the loss functions, the optimum regression statistic for `ACT`, `QLD`, `VIC`, and `WA` is k-nearest neighbours. For `SA`, either median or mean shall suffice. `NT` and `TAS` are more complicated to evaluate as the least error varies among the loss functions. 

The Mean Square Error and the Root Mean Square Error place more weight towards outliers. Considering the distribution plots above, both `NT` and `TAS` have extreme outliers. As the objective is to minimise error between the test and training sets, these loss functions shall be ignored. Naturally, the optimum regression statistic is median for both states. This is based upon the Mean Absolute Error and observed distribution plots.


```r
# Impute missing values in daily test variable with median
for (i in c('NT', 'SA', 'TAS')) {
  covidAU_joined[covidAU_joined$`Province/State` == i, 'Daily Tests'] %<>% 
    impute(fun = median) %>% 
    round(0)
}

# Impute missing values in daily test variable with kNN 
for (i in c('ACT', 'QLD', 'VIC', 'WA')) {
  stateTesti <- covidAU_joined[covidAU_joined$`Province/State` == i, ]
  k <- nrow(stateTesti) %>% sqrt() %>% floor()
  kNNimputed <- kNN(stateTesti, "Daily Tests", k)
  covidAU_joined[covidAU_joined$`Province/State` == i, ] <- kNNimputed[,-7]
}
```

Given that `Daily Tests` has now been correctly imputed in the original data frame, the missing values in `Cumulative Tests` can now be transformed to account for the adjustments made. The function `correctcumtests()` has been created below to perform this task. The procedure of this function is simple to follow with the comments made in the code chunk.


```r
# Cumulative tests can now be correctly adjusted
correctcumtests <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_joined[covidAU_joined$`Province/State` == state, ]
  # A forward loop
  for (i in 1:nrow(x)) {
    if (i == 1) {
      # Assign the first value of daily test for each state as the first value of cumulative test
      x$`Cumulative Tests`[i] <- x$`Daily Tests`[i]
      covidAU_joined[covidAU_joined$`Province/State` == state, 'Cumulative Tests'][i,] <<- x$`Cumulative Tests`[i]
    } else {
      # Correct the values of cumulative test by iteration
      x$`Cumulative Tests`[i] <- x$`Cumulative Tests`[i-1] + x$`Daily Tests`[i]
      covidAU_joined[covidAU_joined$`Province/State` == state, 'Cumulative Tests'][i,] <<- x$`Cumulative Tests`[i]  
    }
  }
}

# Execute correctcumtests on all states
for (i in states) {
  correctcumtests(i)
}
```

The output below shows that no missing values remain in the `Daily Tests` and `Cumulative Tests` columns after imputation.


```r
# Confirm no missing values remain in the data frame
is.na(covidAU_joined) %>% colSums()
```

```
##   Province/State             Date Cumulative Cases Cumulative Tests      Daily Cases      Daily Tests 
##                0                0                0                0                0                0
```

<h2><p align=center>Check for Noise in Time-series Plots</h2>

To check the smoothness of the time-series data, visualisations of the `Daily Cases` and `Daily Tests` variables are plotted below. 

```r
# Reshape covidAU_joined data frame into wide format to create a two variable time series area plot using ggplot2
covidAU_wide <- covidAU_joined %>% gather(`Cumulative Metric`, `Cumulative Value`, 3:4)
covidAU_wide <- covidAU_wide %>% gather(`Daily Metric`, `Daily Value`, 3:4)

# Create time series area plot overlaying daily cases and daily tests
dailycasestests <- ggplot(covidAU_wide, aes(Date, `Daily Value`)) +
                   geom_area(aes(color = `Daily Metric`, fill = `Daily Metric`), 
                             alpha = 0.5, position = position_dodge(0.8)) +
                   scale_y_continuous(trans='log10', labels = comma, oob = squish_infinite) +
                   scale_color_manual(values = c("#08306B", "#4292C6")) +
                   scale_fill_manual(values = c("#08306B", "#4292C6")) +
                   facet_wrap(vars(`Province/State`), scales = "free") +
                   ylab("Value") +
                   ggtitle("COVID-19 Daily Cases and Tests in 2020") +
                   theme(plot.title = element_text(hjust = 0.5, size = 16, vjust = 1, face = "bold"),
                         plot.background = element_rect(fill = "#FFFFFF"),
                         panel.background = element_rect(fill = "#FFFFFF", colour = "#ededed", size = 2, linetype = "solid"),
                         panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#ededed"), 
                         panel.grid.minor = element_blank(),
                         strip.background = element_blank(),
                         axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                         legend.title = element_blank())
ggsave(file="~/COVID-19-AUSTRALIA-PREPROCESSING/R Code for Preprocessing/time_series_cases_tests_original.svg", plot=dailycasestests, width=10, height=8, dpi = 300)
```

<img src="R Code for Preprocessing/time_series_cases_tests_original.svg" width="3000" />

For all states excluding `NT`, there are several discontinuities in `Daily Cases` where values drop to zero rapidly. These features appear noisy on the time-series plots and shall be assumed to be inconsistent with reality. 

A simple algorithm is created below to transform such features into missing values. N.B. only zeros where `Daily Cases[i-1] != 0` and `Daily Cases[i+1] != 0` are transformed, hence ignoring the 'true zeros' in the data frame.


```r
# First, duplicate covidAU_joined into a new data frame.
covidAU_casezeros <- covidAU_joined

# Zeros that are adjacent to non-zero values on both sides are transformed into missing values. Other zeros are ignored to preserve the 'true zeros' in the data frame
zeros <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_casezeros[covidAU_casezeros$`Province/State` == state, ]
  # Forward loop to transform zeros to missing values
  for (i in seq(2, nrow(x-1))) {
    if (x$`Daily Cases`[i] == 0 & x$`Daily Cases`[i-1] != 0 & x$`Daily Cases`[i+1] != 0) {
      covidAU_casezeros[covidAU_casezeros$`Province/State` == state, 'Daily Cases'][i,] <<- NA
    } else {
      invisible()
    }
  }
}

# Execute zeros function for all states    
states <- c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA')
for (i in states) {
  zeros(i)
}
```

A good method to improve noise is to use a k-nearest neighbours algorithm on the transformed missing values.

```r
# Impute transformed missing values in the daily cases variable with a kNN algorithm
for (i in states) {
  stateTesti <- covidAU_casezeros[covidAU_casezeros$`Province/State` == i, ]
  k <- nrow(stateTesti) %>% sqrt() %>% floor()
  kNNimputed <- kNN(stateTesti, "Daily Cases", k)
  covidAU_casezeros[covidAU_casezeros$`Province/State` == i, ] <- kNNimputed[,-7]
}
```

Compare the plots below with those above, the algorithm has clearly improved noise in the `Daily Cases` variable in most states.

```r
# Reshape covidAU_casezeros data frame into wide format to create a two variable time series area plot using ggplot2 with kNN dampening applied
covidAUcasezeros_gathered <- covidAU_casezeros %>% gather(`Cumulative Metric`, `Cumulative Value`, 3:4)
covidAUcasezeros_gathered <- covidAUcasezeros_gathered %>% gather(`Daily Metric`, `Daily Value`, 3:4)

# Create time series area plot overlaying daily cases and daily tests with kNN dampening applied
dailycasestestsdampen <- ggplot(covidAUcasezeros_gathered, aes(Date, `Daily Value`)) +
                         geom_area(aes(color = `Daily Metric`, fill = `Daily Metric`), 
                                        alpha = 0.5, position = position_dodge(0.8)) +
                         scale_y_continuous(trans='log10', labels = comma, oob = squish_infinite) +
                         scale_color_manual(values = c("#08306B", "#4292C6")) +
                         scale_fill_manual(values = c("#08306B", "#4292C6")) +
                         facet_wrap(vars(`Province/State`), scales = "free") +
                         ylab("Value") +
                         ggtitle("COVID-19 Daily Cases and Tests in 2020\n(with kNN dampening)") +
                         theme(plot.title = element_text(hjust = 0.5, size = 16, vjust = 1, face = "bold"),
                            plot.background = element_rect(fill = "#FFFFFF"),
                            panel.background = element_rect(fill = "#FFFFFF", colour = "#ededed", size = 2, linetype = "solid"),
                            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#ededed"), 
                            panel.grid.minor = element_blank(),
                            strip.background = element_blank(),
                            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                            axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                            legend.title = element_blank())
ggsave(file="~/COVID-19-AUSTRALIA-PREPROCESSING/R Code for Preprocessing/time_series_cases_tests_noise_dampened.svg", plot=dailycasestestsdampen, width=10, height=8, dpi = 300)
```

<img src="R Code for Preprocessing/time_series_cases_tests_noise_dampened.svg" width="3000" />

With the noise reduced, the values in `Cumulative Cases` are now transformed to account for the corrections made in `Daily Cases`. Like many of functions created above, the values of `Cumulative Cases` are corrected by the `correctcumcases()` function by iteration. The logic is followed in the code chunk below. 

```r
# Cumulative cases can now be correctly adjusted
correctcumcases <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_casezeros[covidAU_casezeros$`Province/State` == state, ]
  # Forward loop
  for (i in 1:nrow(x)) {
    # Correct the values of cumulative test by iteration after the first value
    if (i > 1) {
      x$`Cumulative Cases`[i] <- x$`Cumulative Cases`[i-1] + x$`Daily Cases`[i]
      covidAU_casezeros[covidAU_casezeros$`Province/State` == state, 'Cumulative Cases'][i,] <<- x$`Cumulative Cases`[i]  
    } else {
      invisible()
    }
  }
}

# Execute correctcumcases function for all states
for (i in states) {
  correctcumcases(i)
}

# Rename data frame to a descriptive name
covidAU_dampened <- covidAU_casezeros
```

<h2><p align=center>Create Moving Averages of COVID-19 Cases</h2>

Daily changes in COVID-19 cases give short term insights. For purposes of data analysis or predictive modelling, this might not yield very insightful results. Coupled alongside daily changes, a moving average should be included to yield longer term insights.

Below is a code chunk to insert a seven day case moving average into the data frame.

```r
# Create seven day case moving average variable and temporarily fill with missing values
covidAU_dampened$`7 Day Case Moving Average` <- as.numeric(NA)

# A function to calculate the seven day case moving average from the daily case values
sevencaseavg <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_dampened[covidAU_dampened$`Province/State` == state, ]
  # Create a forward loop
  for (i in 1:nrow(x)) {
    # First six values for each state will be zero and the values after will be the moving average of the previous seven values of daily cases
    if (i < 7) {
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '7 Day Case Moving Average'] <<- 0
    } else {
      x$`7 Day Case Moving Average`[i] <- mean(x$`Daily Cases`[seq(i-6,i)]) %>% round(0)
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '7 Day Case Moving Average'] <<- x$`7 Day Case Moving Average`[i]
    }
  }
}

# Execute sevencaseavg function for all states
for (i in states) {
  sevencaseavg(i)
}
```

A fourteen day case moving average is also created.

```r
# Create fourteen day case moving average variable and temporarily fill with missing values
covidAU_dampened$`14 Day Case Moving Average` <- as.numeric(NA)

# A function to calculate the fourteen day case moving average from the daily case values
fourteencaseavg <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_dampened[covidAU_dampened$`Province/State` == state, ]
  # Create a forward loop
  for (i in 1:nrow(x)) {
    # First thirteen values for each state will be zero and the values after will be the moving average of the previous fourteen values of daily cases
    if (i < 14) {
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '14 Day Case Moving Average'] <<- 0
    } else {
      x$`14 Day Case Moving Average`[i] <- mean(x$`Daily Cases`[seq(i-13,i)]) %>% round(0)
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '14 Day Case Moving Average'] <<- x$`14 Day Case Moving Average`[i]
    }
  }
}

# Execute fourteencaseavg function for all states
for (i in states) {
  fourteencaseavg(i)
}

# Show new data frame
covidAU_dampened %>% head()
```

```
## # A tibble: 6 x 8
##   `Province/State` Date       `Cumulative Cas~ `Cumulative Tes~ `Daily Cases`
##   <fct>            <date>                <dbl>            <dbl>         <dbl>
## 1 NSW              2020-03-09               48             8371            10
## 2 NSW              2020-03-10               55            10165             7
## 3 NSW              2020-03-11               65            10221            10
## 4 NSW              2020-03-12               92            14856            27
## 5 NSW              2020-03-13              119            16685            27
## 6 NSW              2020-03-14              139            20496            20
## # ... with 3 more variables: `Daily Tests` <dbl>, `7 Day Case Moving
## #   Average` <dbl>, `14 Day Case Moving Average` <dbl>
```

<h2><p align=center>Create Moving Averages of COVID-19 Tests</h2>

Similar to the last section, moving averages should also be created to gauge longer term changes in COVID-19 test numbers. Below is a code chunk to insert a seven day test moving average into the data frame.

```r
# Create seven day test moving average variable and temporarily fill with missing values
covidAU_dampened$`7 Day Test Moving Average` <- as.numeric(NA)

# A function to calculate the seven day test moving average from the daily test values
svndaytest <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_dampened[covidAU_dampened$`Province/State` == state, ]
  # Create a forward loop
  for (i in 1:nrow(x)) {
    # First six values for each state will be zero and the values after will be the moving average of the previous seven values of daily tests
    if (i < 7) {
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '7 Day Test Moving Average'] <<- 0
    } else {
      x$`7 Day Test Moving Average`[i] <- mean(x$`Daily Tests`[seq(i-6,i)]) %>% round(0)
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '7 Day Test Moving Average'] <<- x$`7 Day Test Moving Average`[i]
    }
  }
}

# Execute svndaytest function for all states
for (i in states) {
  svndaytest(i)
}
```

A fourteen day test moving average is also created.

```r
# Create fourteen day test moving average variable and temporarily fill with missing values
covidAU_dampened$`14 Day Test Moving Average` <- as.numeric(NA)
# A function to calculate the fourteen day test moving average from the daily test values
fourteendayavgtest <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_dampened[covidAU_dampened$`Province/State` == state, ]
  # Create a forward loop
  for (i in 1:nrow(x)) {
  # First thirteen values for each state will be zero and the values after will be the moving average of the previous fourteen values of daily tests
    if (i < 14) {
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '14 Day Test Moving Average'] <<- 0
    } else {
      x$`14 Day Test Moving Average`[i] <- mean(x$`Daily Tests`[seq(i-13,i)]) %>% round(0)
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '14 Day Test Moving Average'] <<- x$`14 Day Test Moving Average`[i]
    }
  }
}

# Execute fourteendayavgtest function for all states
for (i in states) {
  fourteendayavgtest(i)
}

# Show new data frame
covidAU_dampened %>% head()
```

```
## # A tibble: 6 x 10
##   `Province/State` Date       `Cumulative Cas~ `Cumulative Tes~ `Daily Cases`
##   <fct>            <date>                <dbl>            <dbl>         <dbl>
## 1 NSW              2020-03-09               48             8371            10
## 2 NSW              2020-03-10               55            10165             7
## 3 NSW              2020-03-11               65            10221            10
## 4 NSW              2020-03-12               92            14856            27
## 5 NSW              2020-03-13              119            16685            27
## 6 NSW              2020-03-14              139            20496            20
## # ... with 5 more variables: `Daily Tests` <dbl>, `7 Day Case Moving
## #   Average` <dbl>, `14 Day Case Moving Average` <dbl>, `7 Day Test Moving
## #   Average` <dbl>, `14 Day Test Moving Average` <dbl>
```

<h2><p align=center>Export to CSV</h2>

A CSV of the final cleaned data frame is created in the chunk below.

```r
# Rename to final copy
covid19_Australia_data_cleaned <- covidAU_dampened

# Write final copy to CSV
write_csv(covid19_Australia_data_cleaned, '~/COVID-19-AUSTRALIA-PREPROCESSING/Cleaned Data after Preprocessing/covid19_Australia_data_cleaned.csv')
```

[View Cleaned Data Set](https://github.com/j-b-ferguson/COVID-19-AUSTRALIA-PREPROCESSING/blob/main/Cleaned%20Data%20after%20Preprocessing/covid19_Australia_data_cleaned.csv)
