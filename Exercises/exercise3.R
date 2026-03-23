## Week 5: Sampling, Correlation, and the Law of Large Numbers

## INFERRING POPULATION CHARACTERISTICS VIA SURVEY RESEARCH

## MEASURING SUPPORT FOR TRUMP IN 2020 US ELECTION POLL

## Set the working directory
## (delete the lines of code that are not for your computer, and
##  if using Windows, replace "username" with your own username)
setwd("C:/Users/nickd/Stevens/EM570/Data-Storytelling-2026/data/") # if Windows

library(dplyr) # loads dplyr package for data manipulation
library(labelled) # loads labelled package for working with labelled data
## Load the dataset
anes <- read.csv("anes_data_2020_big.csv") # reads and stores data


# creates binary variable for intention to vote: 

# Codebook for vote variable:
# "Biden" = 1,
# "Trump" = 2,

anes$vote  <- ifelse(anes$vote_2020 == 2, 1, 0) # creates binary variable for vote: 1 for Trump, 0 for Biden

# Codebook for educ_4 variable:
# "Grade School or less" = 1,
# "High School" = 2,
# "Some College" = 3,
# "College or advanced degree" = 4


anes <- anes |>
  mutate(educ_4_labeled = case_when(
    educ_4 == 1 ~ "Grade School or less",
    educ_4 == 2 ~ "High School",
    educ_4 == 3 ~ "Some College",
    educ_4 == 4 ~ "College or advanced degree",
    TRUE ~ NA_character_
  )) # creates categorical variable for education using dplyr


## Understand the data
## (Read about description of variables and unit of observation)
head(anes) # shows first observations

## Identify the types of variables included
## (character vs. numeric; binary vs. non-binary)

## Identify the number of observations
dim(anes) # provides dimensions of dataframe: rows, columns

## Predicting the referendum outcome
## Frequency table

## Table of proportions 
## option a: create frequency table first
freq_table <- table(anes$vote) # object with frequency table
prop.table(freq_table) # creates table of proportions

## option b: do it all at once
prop.table(table(anes$vote)) # creates table of proportions

## 3.4 WHO SUPPORTED TRUMP?

## Handling missing data

table(anes$educ_4, exclude=NULL) # table() including NAs
mean(anes$vote) # mean() without removing NAs
mean(anes$vote, na.rm=TRUE) # mean() removing NAs
anes1 <- na.omit(anes) # removes observations with NAs
head(anes) # shows first observations of original dataframe
head(anes1) # shows first observations of clean dataframe
dim(anes) # provides dimensions (rows, columns) of original dataframe
dim(anes1) # provides dimensions (rows, columns) of clean dataframe

## Two-way frequency tables
table(anes1$vote, anes1$educ_4_labeled) # creates two-way frequency table of vote and education

## Two-way tables of proportions
prop.table(table(anes1$vote, anes1$educ_4_labeled)) 
prop.table(table(anes1$vote, anes1$educ_4_labeled), margin=1) # with margin=1
prop.table(table(anes1$vote, anes1$educ_4_labeled), margin=2) # with margin=2

## Histograms
hist(anes1$age) # creates histogram of all observations in age
hist(anes1$age[anes1$vote==0]) # creates histogram for Biden supporters
hist(anes1$age[anes1$vote==1]) # creates histogram for Trump supporters

## Density histograms
hist(anes1$age[anes1$educ_4==1]) # (frequency) histogram for respondents w/ grade School or less
hist(anes1$age[anes1$educ_4==4]) # (frequency) histogram for respondents  w/ undergraduate degree or more
hist(anes1$age[anes1$educ_4==1], freq=FALSE) # density histogram for respondents w/ grade School or less
hist(anes1$age[anes1$educ_4==4], freq=FALSE) # density histogram for respondents  w/ undergraduate degree or more

## Descriptive statistics
mean(anes1$age[anes1$vote==0]) # mean age of Biden supporters
mean(anes1$age[anes1$vote==1]) # mean age of Trump supporters
median(anes1$age[anes1$vote==0]) # median age of Biden supporters
median(anes1$age[anes1$vote==1]) # median age of Trump supporters
sd(anes1$age[anes1$vote==0]) # sd of age for Biden supporters
sd(anes1$age[anes1$vote==1]) # sd of age for Trump supporters
var(anes1$age[anes1$vote==1]) # variance of age of Trump supporters
sd(anes1$age[anes1$vote==1])^2 # square of sd of age for Trump supporters
sqrt(var(anes1$age[anes1$vote==1])) # square root of variance of age for Trump supporters


## Scatter plot

anes1$high_education <- ifelse(anes1$educ_4 == 4, 1, 0) # creates binary variable for high education: 1 for college or advanced degree, 0 for less than college

table(anes1$high_education, anes1$vote) # scatter plot X, Y

## Correlation
cor(anes1$high_education, anes1$vote) # calculates correlation between X and Y
cor(anes1$vote, anes1$high_education) # calculates correlation between Y and X

## Law of Large Numbers


sample_means_function <- function(vector, n, num_samples) {
  sample_means <- numeric(num_samples) # creates empty vector to store sample means
  for (i in 1:num_samples) { # for loop to create num_samples samples
    sample <- sample(vector, n, replace=TRUE) # creates a sample of size n with replacement
    sample_means[i] <- mean(sample) # calculates mean of sample and stores it in vector
  }
  return(sample_means) # returns vector of sample means
}

anes1_var <- anes1$vote # creates vector of variable of interest

# Sample 5

sample_means_5 <- sample_means_function(anes1_var, n=5, num_samples=1000) 

hist(sample_means_5) # creates histogram of sample means
abline(v=mean(anes1_var), col="red") # adds vertical line for population mean
abline(v=mean(sample_means_5), col="blue") # adds vertical line for population mean

# Sample 10

sample_means_10 <- sample_means_function(anes1_var, n=10, num_samples=1000) 

hist(sample_means_10) # creates histogram of sample means
abline(v=mean(anes1_var), col="red") # adds vertical line for population mean
abline(v=mean(sample_means_10), col="blue") # adds vertical line for population mean

# Sample 50

sample_means_50 <- sample_means_function(anes1_var, n=50, num_samples=1000) 

hist(sample_means_50) # creates histogram of sample means
abline(v=mean(anes1_var), col="red") # adds vertical line for population mean
abline(v=mean(sample_means_50), col="blue") # adds vertical line for population mean


### Sampling Bias 

## Let's say we only sample from respondents who are from California

anes1_ca <- anes1[anes1$state == "CA", ] # creates subset of data for California respondents

freq_table_ca <- table(anes1_ca$vote) # object with frequency table
freq_table_ca_prop <- prop.table(freq_table_ca)

diff_trump_support <- freq_table_ca_prop[2] - prop.table(freq_table)[2] # difference in proportion of Trump supporters between California and overall sample

# We will see strongly different results if we only sample from California respondents


## Class Exercise: Analyzing the Age of Trump and Biden supporters

# 1. Subset age depending on vote choice (Pick either Trump or Biden supporters)
biden_age <- anes1$age[anes1$vote == 0]

# 2. Create a histogram of the age distribution for your chosen group of supporters
hist(biden_age,
     main = "Age Distribution of Biden Supporters",
     xlab = "Age",
     col = "blue",
     border = "white")

# 3. Calculate the mean, median, and standard deviation of age for your chosen group
mean(biden_age)    # mean age of Biden supporters
median(biden_age)  # median age of Biden supporters
sd(biden_age)      # standard deviation of age for Biden supporters

# 4. Use the Sample Means Function to create a distribution of sample means for age with a sample size of 15 and 1000 samples
biden_sample_means_15 <- sample_means_function(biden_age, n = 15, num_samples = 1000)

hist(biden_sample_means_15,
     main = "Distribution of Sample Means: Biden Supporter Age (n=15)",
     xlab = "Sample Mean Age",
     col = "blue",
     border = "white")
abline(v = mean(biden_age), col = "red", lwd = 2)   # population mean (red)
abline(v = mean(biden_sample_means_15), col = "lightblue", lwd = 2) # mean of sample means (blue)

# 5. Subset your data to only include respondents from a specific state (e.g., Texas) and find the average Trump support rate. Compare the results to the overall sample.
anes1_tx <- anes1[anes1$state == "NJ", ]

trump_support_nj      <- mean(anes1_tx$vote)   # Trump support rate in New Jersey
trump_support_overall <- mean(anes1$vote, na.rm = TRUE) # overall Trump support rate

trump_support_nj       # print New Jersey result
trump_support_overall  # print overall result

diff_nj <- trump_support_nj - trump_support_overall
diff_nj  # positive = New Jersey more pro-Trump than national average


