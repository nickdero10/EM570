################################
# Class: Data Storytelling
# Date: 2/3/2026
# Description: This script contains some basic tests to ensure that your R environment is set up correctly.
################################

# Install basic packages if not already installed

# Note: The way you install packages in R is by using the install.packages() function.
# You can check if a package is installed using the requireNamespace() function.
# You have to use the correct package names as they appear on CRAN (Comprehensive R Archive Network, aka the central database of R packages).
# Example: To install the dplyr package, you would use install.packages("dplyr").

required_packages <- c("dplyr", # A package for data manipulation
                       "ggplot2", # A package for data visualization
                       "tidyr", # A package for data tidying
                       "readr", # A package for reading data
                       "lubridate", # A package for date-time manipulation
                       "rprojroot" # A package for finding files in project directories
                       )


# Install packages if don't have (literally doesn't work with bash)
# for (pkg in required_packages) { # Loop through each required package
#   if (!requireNamespace(pkg, quietly = TRUE)) { # Check if package is installed
#     install.packages(pkg) # Install package if not installed
#   }
# }


## Load the packages to ensure they are installed correctly

## Note: You load packages in R using the library() function.

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)
library(here)

## Set working directory to the location of this script

# Note: You can set the working directory in R using the setwd() function.

setwd(here()) # Sets the working directory to the project root

# You can get the current working directory using the getwd() function.

print(paste("Current working directory is set to:", getwd()))

list.files("data/") # Lists files in the data directory to confirm access

wb_data <- read_csv("data/wb_gdpppp_2024.csv")

print(head(wb_data)) # Print the first few rows of the dataset to confirm it is loaded correctly

## Here's a very simple (ugly) plot of the GDP per capita data to confirm that the data loaded and base R plotting works

hist(wb_data$GDPPPP_2024) # Creates a histogram of the GDP per capita data

## Here's a simple ggplot2 plot to confirm that ggplot2 is working correctly

ggplot(wb_data, aes(x = GDPPPP_2024)) + # aes() maps variables to aesthetics
  geom_histogram() +  # geom_histogram() creates a histogram
  theme_minimal() + # theme_minimal() applies a minimal theme
  labs(title = "National-level GDP per Capita with Purchasing Power Parity, World Bank 2024",
       x = "GDP per Capita (PPP, 2024)") # labs() adds labels to the plot

## Let's take the log to make the distribution more normal

ggplot(wb_data, aes(x = log(GDPPPP_2024))) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "National-level GDP per Capita (log) with Purchasing Power Parity, World Bank 2024",
       x = "Logged GDP per Capita (PPP, 2024)")

## We're starting simple for now, but we'll work on making this plot look better in future lessons!
