rm(list = ls())

library(lubridate, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(tidyr, quietly = TRUE)

states <- c('Alabama', 'New York', 'Colorado', 'Minnesota', 'South Carolina', 'Wyoming', 'New Jersey', 'Nevada', 'Texas', 'Iowa', 'Maine')

grep(pattern = 'a', states, value = TRUE, ignore.case = TRUE)

gsub(pattern = "South", replacement = "North", states[5])

(positions_a <- gregexpr(pattern = "a", text = states, ignore.case = TRUE))

star <- read.csv("C:/Users/nickd/EM570/Data-Storytelling-2026/data/STAR.csv")
head(star)

which(star$classtype == 'small')  # reports indicies       
which(star$reading > 600)

star[which(star$classtype == 'small'), c('classtype', 'reading')]

subset(star, reading > 600, c('classtype', 'reading'))

star |> filter(reading > 600) |> head()

star |> select(classtype, reading) |> head()

star |> filter(reading > 600) |> select(classtype, reading) |> head()

sort(star$reading)  
sort(star)

order(star$reading)  

star[order(star$reading), c('classtype', 'reading')] |> head()

star[order(star$classtype, -star$reading), ] |> head()

star |> arrange(classtype, desc(reading)) |> head()

apply(star[, 2:3], MARGIN = 1, FUN = mean)  
apply(star[, 2:3], MARGIN = 2, FUN = mean)

lapply(star[, 2:3], sd)
sapply(star[, 2:3], sd)

(num_a <- sapply(positions_a, function(x) ifelse(x[1] > 0, length(x), 0)))

tapply(star$reading, star$classtype, summary)

aggregate(star$reading, list(star$classtype), mean)

# Exercise 1: Create this string 'A&1B&2C&3' using a paste function
paste(c("A","B","C"), 1:3, sep="&", collapse="")

# Exercise 2a: Take the following date (November 11, 2011) and turn it into a date vector in R
date <- "November 11, 2011"

# Exercise 2b: Display the date vector in the format (month.day.year')
(lubri_dates <- mdy(date))

# Exercise 3: Create a subset of mydata, which contains the 25 highest v1 scores
v1 <- rnorm(100, 75, 15)
v2 <- as.factor(rep(c("A", "B", "C", "D"), times = 25))
v3 <- rnorm(100, 1, .5) 

mydata <- data.frame(v1, v2, v3)

mydata$v1

top25 <- mydata[order(-mydata$v1), ][1:25, "v1"]

# Exercise 4: Determine if the dataset below is long or wide, and reshape 
data_reshape <- data.frame(State = rep(state.name, 4), year = rep(2010:2013, each = 50), 
                           GSPPC = rnorm(200, mean = 40000, sd = 10000))

data_wide <- reshape(data_reshape, v.names = "GSPPC", idvar = "State", 
                       timevar = "year", direction = "wide") 

tail(data_wide, n = 5)

# Exercise 5: Merge the following data set to data1
(data1 <- data.frame(id = rep(1:5, 3), year = rep(2000:2002, each = 5), 
                     group = sample(c("A", "B", "C"), 15, replace = TRUE)))

(data3 <- data.frame(id = rep(1:5, each = 4), year = rep(2000:2003, 5),
                     response = sample(1:5, size = 20, replace = TRUE)))

data_merge <- merge(data1, data3, by = c("id", "year"), all = TRUE) 

head(data_merge)

# Exercise 6: Using one of the methods above, find the median math score of students by their graduation status
star |>
  group_by(classtype) |>
  summarize(median.math = median(math, na.rm = TRUE))

# Exercise 7: Create a function that identifies the second largest element in a numeric vector







