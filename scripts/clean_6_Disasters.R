#Natural Disasters

# Load the required libraries
library(lubridate)
library(dplyr)
library(readr)

# Read the CSV data file into a data frame named "Disasters"
Disasters <- read.csv("C:\\Users\\twins\\Desktop\\DS OUVRIR\\DS-project\\scripts\\data\\1970-2021_DISASTERS.xlsx - emdat data.csv")

# Convert "Disasters" into a data frame (if it's not already)
Disasters <- as.data.frame(Disasters)

# Select specific columns of interest from the "Disasters" data frame
Disasters <- Disasters %>%
  select(Year, Country, ISO, Location, Continent, Disaster.Subgroup, Disaster.Type, Total.Deaths, No.Injured, No.Affected, No.Homeless, Total.Affected, Total.Damages...000.US..)

# Rearrange the columns, changed the type of data, renamed the columns
Rearanged_Disasters <- Disasters %>%
  filter(Year >= 2000 & Year <= 2022) %>%
  mutate(
    code = as.character(ISO),
    country = as.character(Country),
    year = as.integer(Year),
    continent = as.character(Continent),
    disaster.subgroup = as.character(Disaster.Subgroup),
    disaster.type = as.character(Disaster.Type),
    location = as.character(Location),
    total.deaths = as.numeric(Total.Deaths),
    no.injured = as.numeric(No.Injured),
    no.affected = as.numeric(No.Affected),
    no.homeless = as.numeric(No.Homeless),
    total.affected = as.numeric(Total.Affected),
    total.damages = as.numeric(Total.Damages...000.US..)
  )


# Group the data by "year", "code", "country" and "continent" and summarize the data
Disasters <- Rearanged_Disasters %>%
  group_by(year,code, country, continent) %>%
  summarize(
    total_deaths = sum(total.deaths, na.rm = TRUE),
    no_injured = sum(no.injured, na.rm = TRUE),
    no_affected = sum(no.affected, na.rm = TRUE),
    no_homeless = sum(no.homeless, na.rm = TRUE),
    total_affected = sum(total.affected, na.rm = TRUE),
    total_damages = sum(total.damages, na.rm = TRUE)
  ) 

# Select specific columns from the summarized data and arrange the data by specified columns
disasters <- Disasters %>%
  select(code, country, year, continent, total_deaths, no_injured, no_affected, no_homeless, total_affected, total_damages) %>%
  arrange(code, country, year, continent)
