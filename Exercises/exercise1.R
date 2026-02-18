rm(list=ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)
library(here)

anes_data <- read.csv("C:/users/nickd/EM570/EM570/data/anes_data_2020.csv")

anes_data$age

which(anes_data$age == -1)

anes_data$age[which(anes_data$age == -1 )] <- NA
anes_data$age

is.na(anes_data$age)
table(is.na(anes_data$age))

hist(
     anes_data$age, 
     col = 'blue', 
     breaks = 20, 
     xlab = 'Age', 
     main = 'Age'
)

boxplot(
  age ~ gender,
  data = anes_data,
  col = c('lightblue', 'pink'),
  xlab = 'Gender',
  ylab = 'Age',
  main = 'Age by Gender'
)

table(anes_data$treatment)
prop.table(table(anes_data$treatment))

table(anes_data$partyid, anes_data$immigrants_change6)
prop.table(table(anes_data$partyid, anes_data$immigrants_change6), margin = 1)

