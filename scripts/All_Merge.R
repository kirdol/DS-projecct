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
   liste_de_scripts) # remove the list of scripts from memory)

##### Which countries have many missing observations over the different variables of the different subsets?

#### Question1 
see_missing1_1 <- data_question1 %>%
  group_by(code) %>%
  summarise(across(-c(goal1, goal10),  # Exclude columns "goal1" and "goal10"
                   ~ sum(is.na(.))) %>%
              mutate(num_missing = rowSums(across(everything()))) %>%
              filter(num_missing > 50))
# Remove countries where num_missing >= 50 ??
data_question1_2 <- data_question1 %>% filter(!code %in% see_missing1$code)

see_missing1 <- data_question1 %>%
  group_by(code) %>%
  summarise(across(-c(goal1, goal10),  # Exclude columns "goal1" and "goal10"
                   ~ sum(is.na(.))) %>%
              mutate(num_missing = rowSums(across(everything()))) %>%
              filter(num_missing > 0))
# Delete MilitaryExpenditurePercentGovExp because it has too many missing values and remove the countries of MilitaryExpenditurePercentGDP with more than 25% missings

# Fill in the missings for MilitaryExpenditurePercentGDP and GDPpercapita (mehtod?)

#### Questions 2 and 4
see_missing24 <- data_question24 %>%
  group_by(code) %>%
  summarise(across(everything(), ~ sum(is.na(.))) %>%
              mutate(num_missing = rowSums(across(everything()))) %>%
              filter(num_missing > 0))
# Nothing to remove, only goals 1 and 10 have misisng (already discussed before)

#### Question 3

# Disasters
see_missing3_1 <- data_question3_1 %>%
  group_by(code) %>%
  summarise(across(-c(goal1, goal10),  # Exclude columns "goal1" and "goal10"
                   ~ sum(is.na(.))) %>%
              mutate(num_missing = rowSums(across(everything()))) %>%
              filter(num_missing > 0))
# Many missing, what do we do?

# COVID
see_missing3_2 <- data_question3_2 %>%
  group_by(code) %>%
  summarise(across(-c(goal1, goal10),  # Exclude columns "goal1" and "goal10"
                   ~ sum(is.na(.))) %>%
              mutate(num_missing = rowSums(across(everything()))) %>%
              filter(num_missing > 0))
# No missing

# Conflicts
see_missing3_3 <- data_question3_3 %>%
  group_by(code) %>%
  summarise(across(-c(goal1, goal10),  # Exclude columns "goal1" and "goal10"
    ~ sum(is.na(.))) %>%
    mutate(num_missing = rowSums(across(everything()))) %>%
    filter(num_missing > 0))
# 2 countries have missings, we remove them: MNE and SRB
data_question3_3 <- data_question3_3 %>% filter(!code %in% c("MNE","SRB"))
