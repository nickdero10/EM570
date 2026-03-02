rm(list = ls())

setwd("C:/Users/nickd/EM570/Data-Storytelling-2026/assignments/Assignment1/")

# SECTION 1: EXPLORING THE DATA

# Load data and output first 10 rows
brexit <- read.csv("ps1_BES.csv")
head(brexit, 10)

# How many rows and columns in the dataset
rows <- nrow(brexit)
columns <- ncol(brexit)

print(rows)
print(columns)

# Print the names of the columns in the dataset
print(colnames(brexit))

# What types of data in each column
str(brexit)

# The current output of each column is character, character, character and integer.
# Vote should be factor because each response defines a group. R cant see this without 
# it being defined

# Leave should be numeric, but it reads it as characters because some of it is labeled 
# as missing

# Education should also be numeric because of the 1-5 scale. I think it could also be factor
# but I feel like numeric makes more sense for this dataset

# And age should be numeric because its just respondents ages. It is listed as integer and
# not numeric because each number is whole. Numeric would be output if the values we floats
# or doubles

# This could be fixed after cleaning the data but for now this section just explores


# SECTION 2: CLEANING THE DATA

# How many missing or incorrectly coded values in dataset and fix them
# Check if any spots are blank
na_count <- colSums(is.na(brexit))
print(na_count)

# Check for unique values
lapply(brexit, unique)

# Vote looks good
# Leave and Education both have missing but other than that look fine
# One instance in age showing -9

# "Missing" shows up a lot, so find how many there are in each column and where
missing_count <- sapply(brexit, function(x) sum(tolower(x) == "missing", na.rm = TRUE))
print(missing_count)
which(tolower(brexit$leave) == "missing")

# How many times does -9 show up in age
negative_count <- sapply(brexit, function(x) sum(x == -9, na.rm = TRUE))
print(negative_count)
which(brexit$age == -9)

# "Missing" shows up 2852 times in leave and 3425 times in education
# -9 Shows up 5 times in age
# Change every "missing" and -9 to NA or blank
clean <- function(x) {
  x[tolower(trimws(x)) == "missing"] <- NA
  x[x == -9] <- NA
  return(x)
}

brexit <- as.data.frame(sapply(brexit, clean))


# If any variables are not in the appropriate format, convert them to the correct type
# I mentioned issues for this before in section 1

# Convert vote to factor
brexit$vote <- as.factor(brexit$vote)

# Convert leave to numeric
brexit$leave <- as.numeric(brexit$leave)

# Convert education to factor
brexit$education <- as.numeric(brexit$education)

# Convert age back to numeric (sapply converts everything to characters)
brexit$age <- as.numeric(brexit$age)
str(brexit)


# SECTION 3: ANALYZING THE DATA

# Calculate summary statistics for all numerical columns in the dataset
# Mean for all 3 columns
sapply(brexit[, c("leave", "education", "age")], mean, na.rm = TRUE)

# Median for all 3 columns
sapply(brexit[, c("leave", "education", "age")], median, na.rm = TRUE)

# Standard Deviation for all 3 columns
sapply(brexit[, c("leave", "education", "age")], sd, na.rm = TRUE)

# Leave EU and age boxplot
boxplot(age ~ leave, 
        data = brexit,
        main = "Leaving EU and Age Boxplot",
        xlab = "Leave Vote (0 = Stay, 1 = Leave)",
        ylab = "Age",
        col = c("lightblue", "lightgrey"))

# Correlation Matrix of leave vs age
cor_matrix <- cor(brexit[, c("age", "leave")], use = "complete.obs")
print(cor_matrix)


# The correlation coefficient is 0.24. This means the variables are positively correlated
# because the coefficient is in the positives and not negatives. Because its positively 
# correlated it shows that as age increases, people are more likely to vote to leave. Even 
# though this is the case, the relationship is weak because it is on the lower end of the 
# max of 1.


# SECTION 4: CALCULATING DIFFERENCE IN MEANS
UA <- read.csv("ps1_UA_survey.csv")
print(colnames(brexit))

# Check for issues
na_count_UA <- colSums(is.na(UA))
print(na_count_UA)
lapply(UA, unique)

# Means of both tv and no tv
mean_no_tv <- mean(UA$pro_russian_vote[UA$russian_tv == 0], na.rm = TRUE)
mean_tv <- mean(UA$pro_russian_vote[UA$russian_tv == 1], na.rm = TRUE)

print(mean_no_tv)
print(mean_tv)

# Difference in means
diff_means <- mean_tv - mean_no_tv
print(diff_means)

# The mean of no tv is 17% and the mean of tv is 29%. When finding the difference in means
# it proves that there's a positive relationship between respondents who receive Russian
# TV broadcasts and also voting Pro-Russian.

# Even though respondents who receive Russian tv broadcasts are more likely to vote Pro
# Russian, this can not be considered a causal effect. This is because of a few factors.
# One being that people who are already pro Russian might watch these Russian broadcasts
# without it changing their views. Another thing is also in the dataset with within_25,
# which shows that location plays a big role in the decision too. One way this could be
# classified as a causal effect is if the tv broadcast was administered randomly to people.