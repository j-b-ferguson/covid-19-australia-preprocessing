########################################################################################################################################
# SECTION 1: Packages
########################################################################################################################################

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

########################################################################################################################################
# SECTION 2: Data Summary
########################################################################################################################################

cases <- read_csv('~/COVID-19-AUSTRALIA-PREPROCESSING/Original Untidy Data Sets/time_series_covid19_confirmed_global.csv')
head(cases, 3) # Show first 3 observations of cases data frame

tests <- read_csv('~/COVID-19-AUSTRALIA-PREPROCESSING/Original Untidy Data Sets/Total tests.csv')
head(tests, 3) # Show first 3 observations of tests data frame

########################################################################################################################################
# SECTION 3: Understand Data Frames
########################################################################################################################################

list(class(cases), class(tests)) # Show data structure
list(dim(cases), dim(tests)) # Show data frame dimensions

# Show data types in cases data frame
list(sapply(cases[,1:4], class),
     sapply(cases[,5:273], class) %>% unique() %>% paste('Variables 1/22/20 ... 10/16/20 are all', .))

# Show data types in tests data frame
str(tests, give.attr = FALSE)

########################################################################################################################################
# SECTION 4: Reshape Data Frames
########################################################################################################################################

# Reshape cases data frame from wide to long format
cases_gathered <- gather(cases, key = 'Date', value = 'Cumulative Cases', 5:273)
head(cases_gathered, 3)

# Reshape tests data frame from wide to long format
tests_gathered <- tests %>% gather(key = 'Province/State', value = 'Cumulative Tests', 2:9)
head(tests_gathered, 3)

########################################################################################################################################
# SECTION 5: Convert Data Types
########################################################################################################################################

# Rename first variable of tests_gathered data frame
colnames(tests_gathered)[1] <- 'Date'
names(tests_gathered)

# Change date format to DD/MM/YY format and convert from character to date type in tests_gathered data frame
tests_gathered$Date <- replace(tests_gathered$Date, values = paste0(tests_gathered$Date, '/20'))
tests_gathered$Date <- tests_gathered$Date %>% as.Date(., "%d/%m/%y")

# Convert Province/State to factor type in tests_gathered data frame
tests_gathered$`Province/State` <- tests_gathered$`Province/State` %>% as.factor()

# Check data structure of tests_gathered data frame
str(tests_gathered)

# Subset cases_gathered data frame for observations concerning Australia
cases_gathered <- cases_gathered %>% filter(`Country/Region` == 'Australia') %>% 
  select(`Province/State`, Date, `Cumulative Cases`)

# Check first three observations of subset cases_gathered data frame 
head(cases_gathered, 3)

# Convert Province/State to factor type in cases_gathered data frame and rename variable labels
cases_gathered$`Province/State` <- cases_gathered$`Province/State`%>% as.factor() %>% 
  factor(levels = c('Australian Capital Territory', 'New South Wales', 'Northern Territory', 'Queensland', 'South Australia',                     'Tasmania', 'Victoria', 'Western Australia'),
         labels = c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA'))

# Convert from character to date type in cases_gathered data frame
cases_gathered$Date <- cases_gathered$Date %>% as.Date(., "%m/%d/%y")

# Check data structure of cases_gathered data frame
str(cases_gathered)

########################################################################################################################################
# SECTION 6: Join Data Frames
########################################################################################################################################

# Left join tests_gathered onto cases_gathered by both data sets by "Province/State" and "Date"  
covidAU_joined <- left_join(cases_gathered, tests_gathered, 
                             by = c("Province/State", "Date"))

# Filter cumulative cases greater than zero
covidAU_joined <- covidAU_joined[covidAU_joined$`Cumulative Cases` > 0,]

# Preview new data set
head(covidAU_joined)

########################################################################################################################################
# SECTION 7: Create Variables
########################################################################################################################################

# Insert new variables as NAs
covidAU_joined$`Daily Cases` <- as.numeric(NA)
covidAU_joined$`Daily Tests` <- as.numeric(NA)
covidAU_joined %>% select(`Province/State`, Date, `Daily Cases`, `Daily Tests`) %>% head(3)

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

# Execute correctdailycasetest function over each state
states <- c('ACT', 'NT', 'NSW', 'QLD', 'SA', 'TAS', 'VIC', 'WA')
for (i in states) {
  correctdailycasetest(i)
}

# Filter and show first 10 observations for Victoria
covidAU_joined %>% filter(`Province/State` == 'VIC') %>% head(10)

########################################################################################################################################
# SECTION 8: Correct negative values
########################################################################################################################################

# Count number of observations with cases or tests less than zero
covidAU_joined %>% filter(`Daily Cases` < 0) %>% count()
covidAU_joined %>% filter(`Daily Tests` < 0) %>% count()

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

# Execute correctnegcases and correctnegtests functions over each state
for (i in states) {
  correctnegcases(i)
  correctnegtests(i)
}

# Check to confirm that no negative values remain.
covidAU_joined %>% filter(`Daily Cases` < 0) %>% count()
covidAU_joined %>% filter(`Daily Tests` < 0) %>% count()

########################################################################################################################################
# SECTION 9: Logical Corrections and Pruning
########################################################################################################################################

# Count observations where daily cases are greater than daily tests - a logical inconsistency
covidAU_joined %>% filter(`Daily Cases` > `Daily Tests`) %>% select(`Daily Tests`) %>% count()

# Observations where daily cases are greater than daily tests are assumed erroneous and made missing values
covidAU_joined[!is.na(covidAU_joined$`Daily Tests`) & covidAU_joined$`Daily Tests` < covidAU_joined$`Daily Cases`, 'Daily Tests'] <- NA

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

# Arrange by date
covidAU_joined <- covidAU_joined %>% arrange(Date)

# Find start date first instance
start <- which(covidAU_joined$Date == '2020-10-05')[1]

# Find end date last instance
finish <- which(covidAU_joined$Date == '2020-10-16')[8]

# Daily tests for dates at the end of data frame from 2020-10-05 to 2020-10-16 are missing for all states - remove these
# missing cases
covidAU_joined <- covidAU_joined[-seq(start,finish),]

# Assume that any remaining zeros in daily tests are incorrect. Transform to missing values and impute later
covidAU_joined[covidAU_joined$`Daily Tests` == 0 & !is.na(covidAU_joined$`Daily Tests`), 'Daily Tests'] <- NA

########################################################################################################################################
# SECTION 10: Check Missing Values and Plot Distributions
########################################################################################################################################

# Count of missing values in each variable independent of province/state
is.na(covidAU_joined) %>% colSums()

# Percentage of missing values in each variable for each province/state
for (i in states) {
  statei <- covidAU_joined[covidAU_joined$`Province/State` == i,]
  paste(i, 'missing values') %>% print()
  sapply(statei, function(x) {(sum(is.na(x)) / sum(!is.na(x))) * 100}) %>% print()
}

# Plot matrix of missing values
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

########################################################################################################################################
# SECTION 11: Finding the Optimum Regression Statistic for Imputation
########################################################################################################################################

# NSW has no missing values, so redefine states variable as below
states <- c("ACT", "NT", "QLD", "SA", "TAS", "VIC", "WA")

# Subset the data frame for a machine learning test set, where each province/state has missing values removed from the daily test variable
for (i in states) {
  assign(paste0('testset_', i), covidAU_joined[!is.na(covidAU_joined$`Daily Tests`) & covidAU_joined$`Province/State` == i,])
}

# Calculate the missing value count for each province/state from the covidAU_joined data frame and save as a variable
for (i in states) {
  statetesti <- covidAU_joined[covidAU_joined$`Province/State` == i & is.na(covidAU_joined$`Daily Tests`), 'Daily Tests']
  mvcounti <- count(statetesti)
  assign(paste0('missingvalues_', i), mvcounti$n)
}

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

# Show original count of missing values in covidAU_joined data frame for each province/state
for (i in states) {
  covidAU_joined[covidAU_joined$`Province/State` == i, ]%>% is.na() %>% sum() %>% print()
}

# Compare with training data the count of missing values for each province/state
for (i in states) {
  eval(as.name(paste0('trainingset_', i))) %>% is.na() %>% sum() %>% print()
}

# This code chunk calculates regression statistics to compare the best statistic to use for missing value imputation.
# The statistics used for imputation were either mean, median, or k-nearest neighbours. Regression statistics are the loss 
# functions "mae", "mse", "rmse" and "mape". The statistic with the smallest loss is the preferred method of imputation.
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

########################################################################################################################################
# SECTION 12: Impute Missing Values
########################################################################################################################################

# NSW - no missing values, so no impute necessary
# ACT - kNN imputation has least error according to loss functions, so use kNN imputation
# NT -  Mean has least error according to MAPE, RMSE and MSE, and median has least error according to MAE. The distribution is right-skewed, so use median imputation
# QLD - Use kNN imputation
# SA - Use either mean or median imputation
# TAS - Mean has least error according to RMSE and MSE, but median has least error according to MAE and MAPE. The distribution shows a far-right outlier, so use median imputation
# VIC - Use kNN imputation
# WA - Use kNN imputation

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

# Confirm no missing values remain in the data frame
is.na(covidAU_joined) %>% colSums()

########################################################################################################################################
# SECTION 13: Check for Noise in Time-series Plots
########################################################################################################################################

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

# All time series plots of daily cases (excluding NT) have several discontinuities; i.e. where values drop to zero as an impulse. 
# These discontinuities appear noisy and shall be assumed to be not consistent of reality. An appropriate method to remove this noise is to use a 
# kNN algorithm. 

# First, save covidAU_joined as a new data frame.
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

# Execute imputezeros function for all states    
states <- c('ACT', 'NSW', 'NT', 'QLD', 'SA', 'TAS', 'VIC', 'WA')
for (i in states) {
  zeros(i)
}

# Impute transformed missing values in the daily case variable with a kNN algorithm
for (i in states) {
  stateTesti <- covidAU_casezeros[covidAU_casezeros$`Province/State` == i, ]
  k <- nrow(stateTesti) %>% sqrt() %>% floor()
  kNNimputed <- kNN(stateTesti, "Daily Cases", k)
  covidAU_casezeros[covidAU_casezeros$`Province/State` == i, ] <- kNNimputed[,-7]
}

# Reshape covidAU_casezeros data frame into wide format to create a two variable time series area plot using ggplot2 with kNN damping applied
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
                         ggtitle("COVID-19 Daily Cases and Tests in 2020 (with kNN dampening)") +
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

#kNN algorithm has improved noise in daily case variable in most states

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

# Rename as logical name
covidAU_dampened <- covidAU_casezeros

########################################################################################################################################
# SECTION 14: Create Moving Averages of COVID-19 Cases
########################################################################################################################################

# Create seven day daily case moving average variable and temporarily fill with missing values
covidAU_dampened$`7 day daily case moving average` <- as.numeric(NA)
# A function to calculate the seven day daily case moving average from the daily case values
sevencaseavg <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_dampened[covidAU_dampened$`Province/State` == state, ]
  # Create a forward loop
  for (i in 1:nrow(x)) {
    # First six values for each state will be zero and the values after will be the moving average of the previous seven values of daily cases
    if (i < 7) {
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '7 day daily case moving average'] <<- 0
    } else {
      x$`7 day daily case moving average`[i] <- mean(x$`Daily Cases`[seq(i-6,i)]) %>% round(0)
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '7 day daily case moving average'] <<- x$`7 day daily case moving average`[i]
    }
  }
}

# Execute sevencaseavg function for all states
for (i in states) {
  sevencaseavg(i)
}

# Create fourteen day daily case moving average variable and temporarily fill with missing values
covidAU_dampened$`14 day daily case moving average` <- as.numeric(NA)
# A function to calculate the fourteen day daily case moving average from the daily case values
fourteencaseavg <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_dampened[covidAU_dampened$`Province/State` == state, ]
  # Create a forward loop
  for (i in 1:nrow(x)) {
    # First thirteen values for each state will be zero and the values after will be the moving average of the previous fourteen values of daily cases
    if (i < 14) {
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '14 day daily case moving average'] <<- 0
    } else {
      x$`14 day daily case moving average`[i] <- mean(x$`Daily Cases`[seq(i-13,i)]) %>% round(0)
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '14 day daily case moving average'] <<- x$`14 day daily case moving average`[i]
    }
  }
}

# Execute fourteencaseavg function for all states
for (i in states) {
  fourteencaseavg(i)
}

# Show new data frame
covidAU_dampened %>% head()

########################################################################################################################################
# SECTION 15: Create Moving Averages of COVID-19 Tests
########################################################################################################################################

# Create seven day daily test moving average variable and temporarily fill with missing values
covidAU_dampened$`7 day daily test moving average` <- as.numeric(NA)
# A function to calculate the seven day daily test moving average from the daily test values
svndaytest <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_dampened[covidAU_dampened$`Province/State` == state, ]
  # Create a forward loop
  for (i in 1:nrow(x)) {
    # First six values for each state will be zero and the values after will be the moving average of the previous seven values of daily tests
    if (i < 7) {
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '7 day daily test moving average'] <<- 0
    } else {
      x$`7 day daily test moving average`[i] <- mean(x$`Daily Tests`[seq(i-6,i)]) %>% round(0)
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '7 day daily test moving average'] <<- x$`7 day daily test moving average`[i]
    }
  }
}

# Execute svndaytest function for all states
for (i in states) {
  svndaytest(i)
}

# Create fourteen day daily test moving average variable and temporarily fill with missing values
covidAU_dampened$`14 day daily test moving average` <- as.numeric(NA)
# A function to calculate the fourteen day daily test moving average from the daily test values
fourteendayavgtest <- function(state) {
  # x is a temporary variable that filters the data frame by state
  x <- covidAU_dampened[covidAU_dampened$`Province/State` == state, ]
  # Create a forward loop
  for (i in 1:nrow(x)) {
  # First thirteen values for each state will be zero and the values after will be the moving average of the previous fourteen values of daily tests
    if (i < 14) {
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '14 day daily test moving average'] <<- 0
    } else {
      x$`14 day daily test moving average`[i] <- mean(x$`Daily Tests`[seq(i-13,i)]) %>% round(0)
      covidAU_dampened[covidAU_dampened$`Province/State` == state, ][i, '14 day daily test moving average'] <<- x$`14 day daily test moving average`[i]
    }
  }
}

# Execute fourteendayavgtest function for all states
for (i in states) {
  fourteendayavgtest(i)
}

# Show new data frame
covidAU_dampened %>% head()

########################################################################################################################################
# SECTION 16: Export to CSV
########################################################################################################################################

# Rename to final copy
covid19_Australia_data_cleaned <- covidAU_dampened

# Write final copy to CSV
write_csv(covid19_Australia_data_cleaned, '~/COVID-19-AUSTRALIA-PREPROCESSING/Cleaned Data after Preprocessing/covid19_Australia_data_cleaned.csv')