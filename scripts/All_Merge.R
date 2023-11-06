library(here) # loading package here to be able to execute the other file.
# Pre-cleaning of the datasets
liste_de_scripts <- c("setup.R", # list of all the scripts needed to clean all individual dataset
                      "clean_1_SDG.R",
                      "clean_2_WorldIndex.R",
                      "clean_3_GDPmilitaryExp.R",
                      "clean_4_InternetUsage.R",
                      "clean_5_HumanFreedomIndex.R",
                      "clean_6_Disasters.R",
                      "clean_7_COVID.R",
                      "clean_8_Conflicts.R")

for (script in liste_de_scripts) { # execute each sript
  source(here("scripts", script))}

# merge D1_0_SDG with D2_1_Unemployment_rate 
D2_1_Unemployment_rate$country <- NULL
merge_1_2 <- D1_0_SDG |> left_join(D2_1_Unemployment_rate, join_by(code, year))

# merge merge_1_2 with D3_1_GDP_per_capita, D3_2_Military_Expenditure_Percent_GDP and D3_3_Miliraty_Expenditure_Percent_Gov_Exp
D3_1_GDP_per_capita$country <- NULL
merge_12_3 <- merge_1_2 |> left_join(D3_1_GDP_per_capita, join_by(code, year))

D3_2_Military_Expenditure_Percent_GDP$country <- NULL
merge_12_3 <- merge_12_3 |> left_join(D3_2_Military_Expenditure_Percent_GDP, join_by(code, year)) 

D3_3_Miliraty_Expenditure_Percent_Gov_Exp$country <- NULL
merge_12_3 <- merge_12_3 |> left_join(D3_3_Miliraty_Expenditure_Percent_Gov_Exp, join_by(code, year)) 

# merge merge_12_3 with D4_0_Internet_usage 
D4_0_Internet_usage$country <- NULL
merge_123_4 <- merge_12_3 |> left_join(D4_0_Internet_usage, join_by(code, year)) 

# merge merge_123_4 with D5_0_Human_freedom_index
D5_0_Human_freedom_index$country <- NULL
merge_1234_5 <- merge_123_4 |> left_join(D5_0_Human_freedom_index, join_by(code, year)) 

# merge merge_1234_5 with D_6_0_Disasters
D6_0_Disasters$country <- NULL
merge_12345_6 <- merge_1234_5 |> left_join(D6_0_Disasters, join_by(code, year)) 

# merge merge_12345_6 with D7_0_COVID
D7_0_COVID$country <- NULL
D7_0_COVID <- D7_0_COVID |> distinct(code, year, .keep_all = TRUE)
merge_123456_7 <- merge_12345_6 |> left_join(D7_0_COVID, join_by(code, year)) 

# merge merge_123456_7 with D8_0_Conflicts
D8_0_Conflicts$country <- NULL
all_Merge <- merge_123456_7 |> left_join(D8_0_Conflicts, join_by(code, year)) 

# Filter to delete the countries that were missing from some of our databases
all_Merge <- all_Merge %>% filter(!code %in% missing)

# Replace the NAs of the COVID columns by 0 (because we don't have real missing,
# only introduced by merging for the years before COVID)
all_Merge <- all_Merge %>%
  mutate(
    cases_per_million = ifelse(is.na(cases_per_million), 0, cases_per_million),
    deaths_per_million = ifelse(is.na(deaths_per_million), 0, deaths_per_million),
    stringency = ifelse(is.na(stringency), 0, stringency)
  )

# Complete the values of continent and region

all_Merge <- all_Merge %>%
  group_by(country) %>%
  mutate(continent = ifelse(is.na(continent), first(na.omit(continent)), continent)) %>%
  ungroup()

all_Merge <- all_Merge %>%
  group_by(country) %>%
  mutate(region = ifelse(is.na(region), first(na.omit(region)), region)) %>%
  ungroup()

# Order database
all_Merge <- all_Merge %>%
  select(code, year, country, continent, region, everything())

# subset of data
# for question 1: factors (only until 2020 because no information for freedom index after)
data_question1 <- all_Merge %>% filter(year<=2020) %>% select(-c(total_deaths, no_injured, no_affected, no_homeless, total_affected, total_damages, cases_per_million, deaths_per_million, stringency, ongoing, sum_deaths, pop_affected, area_affected, maxintensity))

# for question 2 and 4: time and relationship between SDGs
data_question24 <- all_Merge %>% select(c(code, year, country, continent, region, overallscore, goal1, goal2, goal3, goal4, goal5, goal6, goal7, goal8, goal9, goal10, goal11, goal12, goal13, goal15, goal16, goal7))

# for question 3: events
# Disasters (only until 2021 because no information for disasters after)
data_question3_1 <- all_Merge %>% filter(year<=2021) %>% select(c(code, year, country, continent, region, overallscore, goal1, goal2, goal3, goal4, goal5, goal6, goal7, goal8, goal9, goal10, goal11, goal12, goal13, goal15, goal16, goal7, total_deaths, no_injured, no_affected, no_homeless, total_affected, total_damages))
# COVID
data_question3_2 <- all_Merge %>% select(c(code, year, country, continent, region, overallscore, goal1, goal2, goal3, goal4, goal5, goal6, goal7, goal8, goal9, goal10, goal11, goal12, goal13, goal15, goal16, goal7, cases_per_million, deaths_per_million, stringency))
# Conflicts (only until 2016 because no information for conflicts after)
data_question3_3 <- all_Merge %>% filter(year<=2016) %>% select(c(code, year, country, continent, region, overallscore, goal1, goal2, goal3, goal4, goal5, goal6, goal7, goal8, goal9, goal10, goal11, goal12, goal13, goal15, goal16, goal7, ongoing, sum_deaths, pop_affected, area_affected, maxintensity))

# cleaning of the environment
rm(merge_1_2, # remove merge_1_2 from memory
   merge_12_3, # remove merge_12_3 from memory
   merge_123_4, # remove merge_123_4 from memory
   merge_1234_5, # remove merge_1234_5 from memory
   merge_12345_6, # remove merge_12345_6 from memory
   merge_123456_7, # remove merge_123456_7 from memory
   liste_de_scripts, #remove the list of scripts from memory
   SDG0, # remove SDG0 from memory
   SDG1, # remove SDG1 from memory
   SDG2, # remove SDG2 from memory
   SDG3, # remove SDG3 from memory
   SDG4, # remove SDG4 from memory
   country_number, # remove country_number from memory
   i, # remove i from memory
   propmissing, # remove propmissing from memory
   proportion, # remove proportion from memory
   makenumSDG, # remove makenumSDG from memory
   list_country,
   conflicts,
   Conflicts,
   country_data,
   country_na_count,
   COVID,
   COVID1,
   COVID2,
   COVID3,
   COVID4,
   data,
   datatibble,
   Disasters,
   Evol_Missing_GDP,
   Evol_Missing_Mil1,
   Evol_Missing_Mil2,
   filtered_data_GDP,
   filtered_data_Mil1,
   filtered_data_Mil2,
   GDPpercapita,
   GDPpercapita1,
   heatmap_ordered,
   MiliratyExpenditurePercentGovExp,
   MiliratyExpenditurePercentGovExp1,
   MilitaryExpenditurePercentGDP,
   MilitaryExpenditurePercentGDP1,
   na_long,
   na_percentage_by_country,
   overall_na_percentage,
   Rearanged_Conflicts,
   Rearanged_Disasters,
   D_2_1_Unemployment_rate_missing_countries,
   interpolated_data,
   issue_list,
   list_code,
   list_country_conflicts,
   list_country_disasters,
   list_country_free,
   list_country_GDP,
   list_country_Unemp,
   missing,
   script,
   cleanwide2long,
   fill_code,
   makenum,
   nameorder,
   remove,
   renameyear,
   wide2long,
   yearint)
