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
data_question1 <- data_question1 %>% filter(!code %in% see_missing1_1$code)

# List of countries deleted
list_country_deleted <- c(unique(see_missing1_1$code))

see_missing1_2 <- data_question1 %>%
  group_by(code) %>%
  summarise(across(-c(goal1, goal10),  # Exclude columns "goal1" and "goal10"
                   ~ sum(is.na(.))) %>%
              mutate(num_missing = rowSums(across(everything()))) %>%
              filter(num_missing > 0))
# Delete MilitaryExpenditurePercentGovExp because it has too many missing values and remove the countries of MilitaryExpenditurePercentGDP with more than 25% missings
data_question1 <- data_question1 %>% select(-MiliratyExpenditurePercentGovExp)

# GDPpercapita
question1_missing_GDP <- data_question1 %>%
  group_by(code) %>%
  summarize(NaGDPpercapita = mean(is.na(GDPpercapita)))%>%
  filter(NaGDPpercapita != 0)
# Only VEN, we can't fill the missing, we delete VEN
data_question1 <- data_question1 %>% filter(code!="VEN")

# Update list countries deleted
list_country_deleted <- c(list_country_deleted, "VEN")

# Military expenditure in % of GDP
question1_missing_Military <- data_question1 %>%
  group_by(code) %>%
  summarize(NaMilitary = mean(is.na(MilitaryExpenditurePercentGDP)))%>%
  filter(NaMilitary != 0)
# Remove the countries with more than 30% missing
data_question1 <- data_question1 %>% filter(code!="BRB" & code!="CRI" & code!="HTI" & code!="ISL" & code!="PAN" & code!="SYR") 

# Update list countries deleted
list_country_deleted <- c(list_country_deleted, "BRB", "CRI", "HTI", "ISL", "PAN", "SYR") 

# Create a dataframe that only have the countries with missing values and 
# add a column which contains the % of missings for each country
question1_missing_Military <- data_question1 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(MilitaryExpenditurePercentGDP))) %>%
  filter(code %in% question1_missing_Military$code)

# See the distribution of the missings per region
Freq_Missing_Military <- ggplot(data = question1_missing_Military) +
  geom_histogram(aes(x = MilitaryExpenditurePercentGDP, 
                     fill = cut(PercentageMissing,
                                breaks = c(0, 0.1, 0.2, 0.3, 1),
                                labels = c("0-10%", "10-20%", "20-30%", "30-100%"))),
                 bins = 30) +
  labs(title = "Military", x = "Military", y = "Frequency") +
  scale_fill_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%"="red","30-100%" = "black"), labels = c("0-10%", "10-20%", "20-30%","30-100%")) +
  guides(fill = guide_legend(title = "% missings")) +
  facet_wrap(~ region, nrow = 3)

print(Freq_Missing_Military)

# All are skewed distributions, we decide to replace the missing values where there are less than 30% missing by the median by region
question1_missing_Military <- question1_missing_Military %>%
  group_by(code, region) %>%
  mutate(
    MilitaryExpenditurePercentGDP = ifelse(
      mean(is.na(MilitaryExpenditurePercentGDP)) < 0.3 & !is.na(MilitaryExpenditurePercentGDP),
      MilitaryExpenditurePercentGDP,
      ifelse(PercentageMissing < 0.3, coalesce(MilitaryExpenditurePercentGDP, median(MilitaryExpenditurePercentGDP, na.rm = TRUE)), MilitaryExpenditurePercentGDP)
    )
  )

data_question1 <- data_question1 %>%
  group_by(code) %>%
  mutate(
    PercentageMissingByCode = mean(is.na(MilitaryExpenditurePercentGDP))
  ) %>%
  ungroup() %>%  # Remove grouping temporarily
  group_by(region) %>%
  mutate(
    MedianByRegion = median(MilitaryExpenditurePercentGDP, na.rm = TRUE),
    MilitaryExpenditurePercentGDP = ifelse(
      PercentageMissingByCode < 0.3 & !is.na(MilitaryExpenditurePercentGDP),
      MilitaryExpenditurePercentGDP,
      ifelse(PercentageMissingByCode < 0.3, MedianByRegion, MilitaryExpenditurePercentGDP)
    )
  ) %>%
  select(-PercentageMissingByCode, -MedianByRegion)

# Internet usage
question1_missing_Internet <- data_question1 %>%
  group_by(code) %>%
  summarize(NaInternet = mean(is.na(`internet usage`)))%>%
  filter(NaInternet != 0)

# Only low % of mising, see distrubtion per region

# Create a dataframe that only have the countries with missing values and 
# add a column which contains the % of missings for each country
question1_missing_Internet <- data_question1 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(`internet usage`))) %>%
  filter(code %in% question1_missing_Internet$code)

# Look at the evolution over the years for the countries that have missing values
Evol_Missing_Internet <- ggplot(data = question1_missing_Internet) +
  geom_point(aes(x = year, y = `internet usage`, 
                 color = cut(PercentageMissing,
                             breaks = c(0, 0.1, 0.2, 0.3, 1),
                             labels = c("0-10%", "10-20%", "20-30%", "30-100%")))) +
  labs(title = "Internet", x = "Year", y = "Internet") +
  scale_color_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%" = "red", "30-100%" = "black"),
                     labels = c("0-10%", "10-20%", "20-30%", "50-100%")) +
  guides(color = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Evol_Missing_Internet)

# Fill with linear interpolation, because lines all evolution increase except for CIV
list_code <- setdiff(unique(question1_missing_Internet$code), "CIV")
for (i in list_code) {
  # Filter the dataset for the current country
  country_data <- data_question1 %>% filter(code == i)
  
  # Perform linear interpolation for the current country's data
  interpolated_data <- na.interp(country_data$`internet usage`)
  
  # Update the original dataset with the interpolated values
  data_question1[data_question1$code == i, "internet usage"] <- interpolated_data
}

# Delete country CIV
data_question1 <- data_question1 %>% filter(code!="CIV")

# Update list countries deleted
list_country_deleted <- c(list_country_deleted, "CIV") 

# Human Freedom Index
# Remove hf_score, pf_score and ef_score because many missings
data_question1 <- data_question1 %>% select(-c(hf_score, pf_score, ef_score))

# pf_law has (many) missing only for one country:BLZ, we remove it 
data_question1 <- data_question1 %>% filter(code!="BLZ")

# Update list countries deleted
list_country_deleted <- c(list_country_deleted, "BLZ") 

# ef_governement: KGZ and SRB have missings -> plot
# KGZ
Evol_Missing_ef_gov <- data_question1 %>% group_by(code) %>% filter(code=="KGZ")
ggplot(Evol_Missing_ef_gov, aes(x = year, y = ef_government)) +
  geom_point() +
  labs(x = "Years", y = "ef_gov")
# Only one missing, in 2000, replace by the value of 2001
# SRB
Evol_Missing_ef_gov <- data_question1 %>% group_by(code) %>% filter(code=="SRB")
ggplot(Evol_Missing_ef_gov, aes(x = year, y = ef_government)) +
  geom_point() +
  labs(x = "Years", y = "ef_gov")
# Only 2 missing, replace by next value
data_question1 <- data_question1 %>%
  mutate(ef_government = ifelse(code == "KGZ" & year == 2000 & is.na(ef_government), ef_government[which(code == "KGZ" & year == 2001)], ef_government))
data_question1 <- data_question1 %>%
  mutate(ef_government = ifelse(code == "SRB" & year == 2000 & is.na(ef_government), ef_government[which(code == "SRB" & year == 2002)], ef_government))
data_question1 <- data_question1 %>%
  mutate(ef_government = ifelse(code == "SRB" & year == 2001 & is.na(ef_government), ef_government[which(code == "SRB" & year == 2002)], ef_government))

# ef_money
question1_missing_ef_money <- data_question1 %>%
  group_by(code) %>%
  summarize(Na_ef_money = mean(is.na(ef_money)))%>%
  filter(Na_ef_money != 0)
# All below 25%

# Create a dataframe that only have the countries with missing values and 
# add a column which contains the % of missings for each country
question1_missing_ef_money <- data_question1 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(ef_money))) %>%
  filter(code %in% question1_missing_ef_money$code)

# Look at the evolution over the years for the countries that have missing values
Evol_Missing_ef_money <- ggplot(data = question1_missing_ef_money) +
  geom_point(aes(x = year, y = ef_money, 
                 color = cut(PercentageMissing,
                             breaks = c(0, 0.1, 0.2, 0.3, 1),
                             labels = c("0-10%", "10-20%", "20-30%", "30-100%")))) +
  labs(title = "ef_money", x = "Year", y = "ef_money") +
  scale_color_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%" = "red", "30-100%" = "black"),
                     labels = c("0-10%", "10-20%", "20-30%", "50-100%")) +
  guides(color = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Evol_Missing_ef_money)

# Linear interpolation for "ARM", "BFA", "BIH", "GEO", "KAZ", "LSO", "MDA", "MKD"
list_code <- c("ARM", "BFA", "BIH", "GEO", "KAZ", "LSO", "MDA", "MKD")
for (i in list_code) {
  # Filter the dataset for the current country
  country_data <- data_question1 %>% filter(code == i)
  
  # Perform linear interpolation for the current country's data
  interpolated_data <- na.interp(country_data$ef_money)
  
  # Update the original dataset with the interpolated values
  data_question1[data_question1$code == i, "ef_money"] <- interpolated_data
}

# ReCreate a dataframe that only have the countries with missing values and 
# add a column which contains the % of missings for each country
question1_missing_ef_money <- data_question1 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(ef_money))) #%>%
#  filter(PercentageMissing !=0)

# See the distribution of the missings per region
Freq_Missing_ef_money <- ggplot(data = question1_missing_ef_money) +
  geom_histogram(aes(x = ef_money, 
                     fill = cut(PercentageMissing,
                                breaks = c(0, 0.1, 0.2, 0.3, 1),
                                labels = c("0-10%", "10-20%", "20-30%", "30-100%"))),
                 bins = 200) +
  labs(title = "ef_money", x = "ef_money", y = "Frequency") +
  scale_fill_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%"="red","30-100%" = "black"), labels = c("0-10%", "10-20%", "20-30%","30-100%")) +
  guides(fill = guide_legend(title = "% missings")) +
  facet_wrap(~ region, nrow = 3)

print(Freq_Missing_ef_money)
# To be cont


# ef_trade
question1_missing_ef_trade <- data_question1 %>%
  group_by(code) %>%
  summarize(Na_ef_trade = mean(is.na(ef_trade)))%>%
  filter(Na_ef_trade != 0)
# All below 25%

# Create a dataframe that only have the countries with missing values and 
# add a column which contains the % of missings for each country
question1_missing_ef_trade <- data_question1 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(ef_trade))) %>%
  filter(code %in% question1_missing_ef_trade$code)

# Look at the evolution over the years for the countries that have missing values
Evol_Missing_ef_trade <- ggplot(data = question1_missing_ef_trade) +
  geom_point(aes(x = year, y = ef_trade, 
                 color = cut(PercentageMissing,
                             breaks = c(0, 0.1, 0.2, 0.3, 1),
                             labels = c("0-10%", "10-20%", "20-30%", "30-100%")))) +
  labs(title = "ef_trade", x = "Year", y = "ef_trade") +
  scale_color_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%" = "red", "30-100%" = "black"),
                     labels = c("0-10%", "10-20%", "20-30%", "50-100%")) +
  guides(color = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Evol_Missing_ef_trade)

# Linear interpolation for "AZE", "BFA", "ETH", "GEO", "VNH"
list_code <- c("AZE", "BFA", "ETH", "GEO", "VNH")
for (i in list_code) {
  # Filter the dataset for the current country
  country_data <- data_question1 %>% filter(code == i)
  
  # Perform linear interpolation for the current country's data
  interpolated_data <- na.interp(country_data$ef_trade)
  
  # Update the original dataset with the interpolated values
  data_question1[data_question1$code == i, "ef_trade"] <- interpolated_data
}

# To be cont

# ef_regulation
question1_missing_ef_regulation <- data_question1 %>%
  group_by(code) %>%
  summarize(Na_ef_regulation = mean(is.na(ef_regulation)))%>%
  filter(Na_ef_regulation != 0)
# All below 25%

# Create a dataframe that only have the countries with missing values and 
# add a column which contains the % of missings for each country
question1_missing_ef_regulation <- data_question1 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(ef_regulation))) %>%
  filter(code %in% question1_missing_ef_regulation$code)

# Look at the evolution over the years for the countries that have missing values
Evol_Missing_ef_regulation <- ggplot(data = question1_missing_ef_regulation) +
  geom_point(aes(x = year, y = ef_regulation, 
                 color = cut(PercentageMissing,
                             breaks = c(0, 0.1, 0.2, 0.3, 1),
                             labels = c("0-10%", "10-20%", "20-30%", "30-100%")))) +
  labs(title = "ef_regulation", x = "Year", y = "ef_regulation") +
  scale_color_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%" = "red", "30-100%" = "black"),
                     labels = c("0-10%", "10-20%", "20-30%", "50-100%")) +
  guides(color = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Evol_Missing_ef_regulation)

# Linear interpolation for "ETH", "KAZ", "MDA", "SRB"
list_code <- c("ETH", "KAZ", "MDA", "SRB")
for (i in list_code) {
  # Filter the dataset for the current country
  country_data <- data_question1 %>% filter(code == i)
  
  # Perform linear interpolation for the current country's data
  interpolated_data <- na.interp(country_data$ef_regulation)
  
  # Update the original dataset with the interpolated values
  data_question1[data_question1$code == i, "ef_regulation"] <- interpolated_data
}

# To be cont


###### TEST #####

test <- data_question1 %>% filter(year>=2005)
mean(is.na(test))

see_missing_test <- test %>%
  group_by(code) %>%
  summarise(across(-c(goal1, goal10),  # Exclude columns "goal1" and "goal10"
                   ~ sum(is.na(.))) %>%
              mutate(num_missing = rowSums(across(everything()))) %>%
              filter(num_missing > 0))

##### No more missing, if we remove years 2000-2005 #####

#### Questions 2 and 4
see_missing24 <- data_question24 %>%
  group_by(code) %>%
  summarise(across(everything(), ~ sum(is.na(.))) %>%
              mutate(num_missing = rowSums(across(everything()))) %>%
              filter(num_missing > 0))
# Nothing to remove, only goals 1 and 10 have missing (already discussed before)

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

