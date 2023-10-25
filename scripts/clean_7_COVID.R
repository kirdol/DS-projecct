#Import data

COVID <- read.csv(here("scripts","data","COVID.csv"))

# Only keep the variables that we are interested in

COVID <- COVID[,c("iso_code", "location", "date", "new_cases_per_million", "new_deaths_per_million", "stringency_index")]

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

# Only have 1 obs per country per year

COVID <- COVID %>%
  group_by(location, date) %>%
  distinct(date, .keep_all = TRUE) %>%
  ungroup()

# Remove the variable that have the information for every day and only keep those by year

COVID <- COVID %>% select(-c(new_cases_per_million, new_deaths_per_million, stringency_index))

# Rename the variables

colnames(COVID) <- c("code", "country", "year", "cases_per_million", "deaths_per_million", "stringency")

# Remove the years after 2022 to match our main database 

COVID <- COVID[COVID$year <= 2022, ]

# Make sure the country codes are all iso codes with 3 letters (we observe that sometimes they are preceded by "OWID_")

COVID$code <- gsub("OWID_", "", COVID$code)

# Investigation of the missing values

mean(is.na(COVID$cases_per_million))
mean(is.na(COVID$deaths_per_million))
mean(is.na(COVID$stringency))

# No missing values except in for the stringency, where there are 4.19% 

# Standardize the country code

COVID$code <- countrycode(
  sourcevar = COVID$code,
  origin = "iso3c",
  destination = "iso3c",
)

# Remove the observations of countries that aren't in our main dataset on SDGs: 

COVID <- COVID %>% filter(code %in% list_country)
length(unique(COVID$code))

# All the 166 countries that we have in the main SDG dataset are also in this one.