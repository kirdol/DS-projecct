COVID <- read.csv('C:/Users/Delia/Documents/1. Formations études école/HEC/MSc1/Data Science in Business Analytics/DS-projecct/COVID.csv')

# Import data

# COVID <- read.csv(here::here('COVID.csv'))

# libraries

library(lubridate)
library(dplyr)

# Only keep the variables that we are interested in

COVID <- COVID[,c("iso_code", "location", "date", "continent", "new_cases_per_million", "new_deaths_per_million", "stringency_index")]

# Transform the dates ("YYYY-MM-DD") into years ("YYYY") and integers

COVID$date <- as.integer(year(COVID$date))

# Aggregate the observation of all days of a year in one observation per country

COVID <- COVID %>%
  group_by(location, date) %>%
  mutate(
    cases_per_million = sum(new_cases_per_million, na.rm = TRUE),
    deaths_per_million = sum(new_deaths_per_million, na.rm = TRUE),
    stringency = mean(stringency_index, na.rm = TRUE)
  )%>%
  ungroup()

COVID <- COVID %>%
  group_by(location, date) %>%
  distinct(date, .keep_all = TRUE) %>%
  ungroup()

# Remove the variable that have the information for every day and only keep those by year

COVID <- subset(COVID, select = -c(new_cases_per_million, new_deaths_per_million, stringency_index))

# Rename the variables

colnames(COVID) <- c("code", "country", "year", "continent", "cases_per_million", "deaths_per_million", "stringency")

# Take the average stringency over the years for a country because of missing values

mean(is.na(COVID$stringency))

COVID <- COVID %>%
  group_by(country) %>%
  mutate(
    stringency = mean(stringency, na.rm = TRUE)
  )%>%
  ungroup()

mean(is.na(COVID$stringency))

# Remove the years after 2022 to match our main database 

COVID <- COVID[COVID$year <= 2022, ]

# Make sure the country codes are all iso codes with 3 letters (we observe that sometimes they are preceded by "OWID_")

COVID$code <- gsub("OWID_", "", COVID$code)

# Transform the continent variable (chr) into a factor

COVID$continent <- as.factor(COVID$continent)
