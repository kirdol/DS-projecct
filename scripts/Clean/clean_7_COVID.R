#Import data

COVID <- read.csv(here("scripts","data","COVID.csv"))

# Only keep the variables that we are interested in

COVID <- COVID[,c("iso_code", "location", "date", "new_cases_per_million", "new_deaths_per_million", "stringency_index")]

# Transform the dates ("YYYY-MM-DD") into years ("YYYY") and integers

COVID$date <- as.integer(year(COVID$date))

# Investigate missing values before aggregating

COVID1 <- COVID %>%
  group_by(iso_code) %>%
  summarize(NaCOVID = round(mean(is.na(new_cases_per_million)),3)) %>%
  filter(NaCOVID != 0)

print(table(COVID1$NaCOVID))

COVID2 <- COVID %>%
  group_by(iso_code) %>%
  summarize(NaCOVID = round(mean(is.na(new_deaths_per_million)),3)) %>%
  filter(NaCOVID != 0)

print(table(COVID2$NaCOVID))

# We see that for each country, we have either only missing values, either a very low % of 
# missing ~1% -> we can compute the sum over each year and ignore the missing values without
# altering the data

COVID3 <- COVID %>%
  group_by(iso_code) %>%
  summarize(NaCOVID = round(mean(is.na(stringency_index)), 3)) %>%
  filter(NaCOVID != 0)

print(table(COVID3$NaCOVID))

# Here we have 3 scenarios: 
# 1) ~20% missings -> ok to ignore missings when computing the mean to have an idea of stringency each year
# 2) all missings -> ok to ignore missings when computing the mean because it will still return a missing value
# 3) almost all are missing: here the mean doesn't make sense -> we will replace the values by NAs to be coherent
# The countries with this issues are: ERI, GUM, PRI and VIR, we want to see if these countries are in our main dataset

issue_list <- c("ERI", "GUM", "PRI", "VIR")
is.element(issue_list, list_country)

# Since non of these countries are in the main SDG database, we can ignore the issue, the lines will be remove later anyway

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

##### Investigation of the missing values #####

mean(is.na(COVID$cases_per_million))
mean(is.na(COVID$deaths_per_million))
mean(is.na(COVID$stringency))

# No missing values except in for the stringency, where there are 4.19% 

COVID4 <- COVID %>%
  group_by(code) %>%
  summarize(NaCOVID = mean(is.na(stringency))) %>%
  filter(NaCOVID != 0)
print(COVID4, n = 300)

# Either all values are missing for one country, or 50% are missing, so these 7 countries 
# won't be included when analysing the effect of stringency

D7_0_COVID <- COVID