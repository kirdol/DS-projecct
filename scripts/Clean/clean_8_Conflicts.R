#Conflicts

# Read the Excel data file into a data frame named "Conflicts"
Conflicts <- read.csv(here("scripts","data","Conflicts.csv"))

# Convert "Conflicts" into a data frame (if it's not already)
Conflicts <- as.data.frame(Conflicts)

# Select specific columns of interest from the "Conflicts" data frame
Conflicts <- Conflicts %>%
  select(year, country, ongoing, gwsum_bestdeaths, pop_affected, peaceyearshigh, area_affected, maxintensity, maxcumulativeintensity)

# Filter rows based on the "year" column
#Note: our data set has no more information about conflicts per country from 2016. As we consider conflicts as events, we will only take into account results between 2000 and 2016.
Rearanged_Conflicts <- Conflicts %>%
  filter(year >= 2000 & year <= 2022)%>%
  mutate(
    ongoing = as.integer(ongoing),
    country = as.character(country),
    year = as.integer(year),
    gwsum_bestdeaths = as.numeric(gwsum_bestdeaths),
    pop_affected = as.numeric(pop_affected),
    area_affected = as.numeric(area_affected),
    maxintensity = as.numeric(maxintensity),
    )

# Group the data by "year", "country" and summarize the data
Conflicts <- Rearanged_Conflicts %>%
  group_by(year, country) %>%
  summarize(
    ongoing = sum (ongoing, na.rm = TRUE),
    sum_deaths = sum(gwsum_bestdeaths, na.rm = TRUE),
    pop_affected = sum(pop_affected, na.rm = TRUE),
    area_affected = sum(area_affected, na.rm = TRUE),
    maxintensity = sum(maxintensity, na.rm = TRUE),
  )
    
# Select specific columns from the summarized data and arrange the data by specified columns
conflicts <- Conflicts %>%
  select(country, year, ongoing, sum_deaths, pop_affected, area_affected, maxintensity) %>%
  arrange(country, year)


# Print the summary of the "Rearanged_Conflicts" data frame
#summary(conflicts)

# Make sure the encoding of the country names are UTF-8
conflicts$country <- iconv(conflicts$country, to = "UTF-8", sub = "byte")

# standardize country names
conflicts <- conflicts %>%
  mutate(country = countrycode(country, "country.name", "country.name"))

# Merge by country name
conflicts <- conflicts %>%
  left_join(D1_0_SDG_country_list, by = "country")

#Rearrange the data
conflicts <- conflicts %>%
  select(code, country, year, ongoing, sum_deaths, pop_affected, area_affected, maxintensity) %>%
  arrange(code, country, year)

# Keep only the countries that are in our main dataset

D8_0_Conflicts <- conflicts %>% filter(code %in% list_country)
(length(unique(conflicts$code)))

# See which ones are missing

list_country_conflicts <- c(unique(conflicts$code))
(missing <- c(missing, setdiff(list_country, list_country_conflicts)))

# Only one country missing that wasn't in the inital conflicts database: BLR