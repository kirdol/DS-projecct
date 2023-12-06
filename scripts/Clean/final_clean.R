# Import merged database
all_Merge <- read.csv(here("scripts","data","all_Merge.csv"))

goal_vars <- all_Merge %>%
  select(starts_with("goal")) %>%
  filter_all(all_vars(!is.na(.))) %>%
  colnames()

# Create a new column "Goals without NAs" in the data frame
to_plot_missing <- all_Merge %>%
  mutate(Goals_without_NAs = rowSums(!is.na(select(., all_of(goal_vars))))) %>%
  select(-c(goal2, goal3, goal4, goal5, goal6, goal7, goal8, goal9, goal11, goal12, goal13, goal15, goal16, goal17))

# Now, you can use the `vis_dat` function with the updated data frame
vis_dat(to_plot_missing, warn_large_data = FALSE) + scale_fill_brewer(palette = "Paired")

# subset of data
# for question 1: factors (only until 2020 because no information for freedom index after)
data_question1 <- all_Merge %>% filter(year<=2020) %>% select(-c(total_deaths, no_injured, no_affected, no_homeless, total_affected, total_damages, cases_per_million, deaths_per_million, stringency, ongoing, sum_deaths, pop_affected, area_affected, maxintensity))

# for question 2 and 4: time and relationship between SDGs
data_question24 <- all_Merge %>% dplyr::select(c(code, year, country, continent, region, overallscore, goal1, goal2, goal3, goal4, goal5, goal6, goal7, goal8, goal9, goal10, goal11, goal12, goal13, goal15, goal16, goal17))

# for question 3: events
# Disasters (only until 2021 because no information for disasters after)
data_question3_1 <- all_Merge %>% filter(year<=2021) %>% select(c(code, year, country, continent, region, overallscore, goal1, goal2, goal3, goal4, goal5, goal6, goal7, goal8, goal9, goal10, goal11, goal12, goal13, goal15, goal16, goal7, total_deaths, no_injured, no_affected, no_homeless, total_affected, total_damages))
# COVID
data_question3_2 <- all_Merge %>% select(c(code, year, country, continent, region, overallscore, goal1, goal2, goal3, goal4, goal5, goal6, goal7, goal8, goal9, goal10, goal11, goal12, goal13, goal15, goal16, goal7, cases_per_million, deaths_per_million, stringency))
# Conflicts (only until 2016 because no information for conflicts after)
data_question3_3 <- all_Merge %>% filter(year<=2016) %>% select(c(code, year, country, continent, region, overallscore, goal1, goal2, goal3, goal4, goal5, goal6, goal7, goal8, goal9, goal10, goal11, goal12, goal13, goal15, goal16, goal7, ongoing, sum_deaths, pop_affected, area_affected, maxintensity))

##### Which countries have many missing observations over the different variables of the different subsets?

#### Question1
data_question1 <- data_question1 %>% select(-X)
 
variable_names <- names(data_question1)
missing_percentages <- sapply(data_question1, function(col) mean(is.na(col)) * 100)

missing_data_summary <- data.frame(
  Variable = variable_names,
  Missing_Percentage = missing_percentages
)

missing_data_summary <- missing_data_summary %>%
  mutate(VariableGroup = ifelse(startsWith(Variable, "goal") & Missing_Percentage == 0, "Goals without NAs", as.character(Variable)))

ggplot(data = missing_data_summary, aes(x = reorder(VariableGroup, Missing_Percentage), y = Missing_Percentage, fill = Missing_Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Missing_Percentage > 1, sprintf("%.1f%%", Missing_Percentage), ""),
                y = Missing_Percentage),
            position = position_stack(vjust = 1),  # Adjust vertical position
            color = "white",  # Text color
            size = 3,          # Text size
            hjust = 1.05) +
  labs(title = "Percentage of Missing Values by Variable",
       x = "Variable",
       y = "Missing Percentage") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  coord_flip()

see_missing1_1 <- data_question1 %>%
  group_by(code) %>%
  summarise(across(-c(year, country, continent, region, population, overallscore, goal1, goal2, goal3, goal4, goal5, goal6, goal7, goal8, goal9, goal10, goal11, goal12, goal13, goal15, goal16, goal17),  
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
# Delete MilitaryExpenditurePercentGovExp because it has too many missing values and remove the countries of MilitaryExpenditurePercentGDP
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
  ungroup() %>%
  group_by(region) %>%
  filter(sum(PercentageMissing, na.rm = TRUE) > 0)

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
  summarize(NaInternet = mean(is.na(internet_usage)))%>%
  filter(NaInternet != 0)

# Only low % of missing

# Create a dataframe that only have the countries with missing values and 
# add a column which contains the % of missings for each country
question1_missing_Internet <- data_question1 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(internet_usage))) %>%
  filter(code %in% question1_missing_Internet$code)

# Look at the evolution over the years for the countries that have missing values
Evol_Missing_Internet <- ggplot(data = question1_missing_Internet) +
  geom_point(aes(x = year, y = internet_usage, 
                 color = cut(PercentageMissing,
                             breaks = c(0, 0.1, 0.2, 0.3, 1),
                             labels = c("0-10%", "10-20%", "20-30%", "30-100%")))) +
  labs(title = "Internet", x = "Year", y = "Internet") +
  scale_color_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%" = "red", "30-100%" = "black"),
                     labels = c("0-10%", "10-20%", "20-30%", "50-100%")) +
  guides(color = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Evol_Missing_Internet)

# Fill with linear interpolation, because all evolution are in an increasing way and are almost straight lines, except for CIV
list_code <- setdiff(unique(question1_missing_Internet$code), "CIV")
for (i in list_code) {
  # Filter the dataset for the current country
  country_data <- data_question1 %>% filter(code == i)
  
  # Perform linear interpolation for the current country's data
  interpolated_data <- na.interp(country_data$internet_usage)
  
  # Update the original dataset with the interpolated values
  data_question1[data_question1$code == i, "internet_usage"] <- interpolated_data
}

# Delete country CIV
data_question1 <- data_question1 %>% filter(code!="CIV")

# Update list countries deleted
list_country_deleted <- c(list_country_deleted, "CIV") 

# Human Freedom Index
# Remove hf_score, pf_score and ef_score because many missing and since these variables are summary of other ones, deleting the mwon't make us loose information
data_question1 <- data_question1 %>% select(-c(hf_score, pf_score, ef_score))

naniar::gg_miss_upset(data_question1, nsets=10, nintersects=11)

# pf_law has (many) missing only for one country:BLZ, we remove it 
data_question1 <- data_question1 %>% filter(code!="BLZ")

# Update list countries deleted
list_country_deleted <- c(list_country_deleted, "BLZ") 

# ef_governement: KGZ and SRB have missing values -> plot
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

# Create a dataframe that only have the countries with missing values and 
# add a column which contains the % of missings for each country
question1_missing_ef_money <- data_question1 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(ef_money))) %>%
  ungroup() %>%
  group_by(region) %>%
  filter(sum(PercentageMissing, na.rm = TRUE) > 0)

# See the distribution of the missings per region
Freq_Missing_ef_money <- ggplot(data = question1_missing_ef_money) +
  geom_histogram(aes(x = ef_money, 
                     fill = cut(PercentageMissing,
                                breaks = c(0, 0.1, 0.2, 0.3, 1),
                                labels = c("0-10%", "10-20%", "20-30%", "30-100%"))),
                 bins = 30) +
  labs(title = "ef_money", x = "ef_money", y = "Frequency") +
  scale_fill_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%"="red","30-100%" = "black"), labels = c("0-10%", "10-20%", "20-30%","30-100%")) +
  guides(fill = guide_legend(title = "% missings")) +
  facet_wrap(~ region, nrow = 3)

print(Freq_Missing_ef_money)

# All are skewed distributions, we decide to replace the missing values where there are less than 30% missing by the median by region

data_question1 <- data_question1 %>%
  group_by(code) %>%
  mutate(
    PercentageMissingByCode = mean(is.na(ef_money))
  ) %>%
  ungroup() %>%  # Remove grouping temporarily
  group_by(region) %>%
  mutate(
    MedianByRegion = median(ef_money, na.rm = TRUE),
    ef_money = ifelse(
      PercentageMissingByCode < 0.3 & !is.na(ef_money),
      ef_money,
      ifelse(PercentageMissingByCode < 0.3, MedianByRegion, ef_money)
    )
  ) %>%
  select(-PercentageMissingByCode, -MedianByRegion)

# ef_trade
question1_missing_ef_trade <- data_question1 %>%
  group_by(code) %>%
  summarize(Na_ef_trade = mean(is.na(ef_trade)))%>%
  filter(Na_ef_trade != 0)
# All below 25%

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

# Create a dataframe that only have the countries with missing values and 
# add a column which contains the % of missings for each country
question1_missing_ef_trade <- data_question1 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(ef_trade))) %>%
  ungroup() %>%
  group_by(region) %>%
  filter(sum(PercentageMissing, na.rm = TRUE) > 0)

# See the distribution of the missings per region
Freq_Missing_ef_trade <- ggplot(data = question1_missing_ef_trade) +
  geom_histogram(aes(x = ef_trade, 
                     fill = cut(PercentageMissing,
                                breaks = c(0, 0.1, 0.2, 0.3, 1),
                                labels = c("0-10%", "10-20%", "20-30%", "30-100%"))),
                 bins = 30) +
  labs(title = "ef_trade", x = "ef_trade", y = "Frequency") +
  scale_fill_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%"="red","30-100%" = "black"), labels = c("0-10%", "10-20%", "20-30%","30-100%")) +
  guides(fill = guide_legend(title = "% missings")) +
  facet_wrap(~ region, nrow = 3)

print(Freq_Missing_ef_trade)

# All are skewed distributions, we decide to replace the missing values where there are less than 30% missing by the median by region

data_question1 <- data_question1 %>%
  group_by(code) %>%
  mutate(
    PercentageMissingByCode = mean(is.na(ef_trade))
  ) %>%
  ungroup() %>%  # Remove grouping temporarily
  group_by(region) %>%
  mutate(
    MedianByRegion = median(ef_trade, na.rm = TRUE),
    ef_trade = ifelse(
      PercentageMissingByCode < 0.3 & !is.na(ef_trade),
      ef_trade,
      ifelse(PercentageMissingByCode < 0.3, MedianByRegion, ef_trade)
    )
  ) %>%
  select(-PercentageMissingByCode, -MedianByRegion)

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

# Create a dataframe that only have the countries with missing values and 
# add a column which contains the % of missings for each country
question1_missing_ef_regulation <- data_question1 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(ef_regulation))) %>%
  ungroup() %>%
  group_by(region) %>%
  filter(sum(PercentageMissing, na.rm = TRUE) > 0)

# See the distribution of the missings per region
Freq_Missing_ef_regulation <- ggplot(data = question1_missing_ef_regulation) +
  geom_histogram(aes(x = ef_regulation, 
                     fill = cut(PercentageMissing,
                                breaks = c(0, 0.1, 0.2, 0.3, 1),
                                labels = c("0-10%", "10-20%", "20-30%", "30-100%"))),
                 bins = 100) +
  labs(title = "ef_regulation", x = "ef_regulation", y = "Frequency") +
  scale_fill_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%"="red","30-100%" = "black"), labels = c("0-10%", "10-20%", "20-30%","30-100%")) +
  guides(fill = guide_legend(title = "% missings")) +
  facet_wrap(~ region, nrow = 3)

print(Freq_Missing_ef_regulation)

# All are skewed distributions, we decide to replace the missing values where there are less than 30% missing by the median by region

data_question1 <- data_question1 %>%
  group_by(code) %>%
  mutate(
    PercentageMissingByCode = mean(is.na(ef_regulation))
  ) %>%
  ungroup() %>%  # Remove grouping temporarily
  group_by(region) %>%
  mutate(
    MedianByRegion = median(ef_regulation, na.rm = TRUE),
    ef_regulation = ifelse(
      PercentageMissingByCode < 0.3 & !is.na(ef_regulation),
      ef_regulation,
      ifelse(PercentageMissingByCode < 0.3, MedianByRegion, ef_regulation)
    )
  ) %>%
  select(-PercentageMissingByCode, -MedianByRegion)%>%
  ungroup()

#### Visualization NA Q1 HUMAN FREEDOM INDEX  #### (not just human freedom index also other variables :))
na_counts <- data_question1 %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "na_{.col}")) %>%
  pivot_longer(cols = starts_with("na_"), names_to = "column", values_to = "na_count", names_prefix = "na_")

na_counts

######## NO MORE MISSINGS HUMAN FREEDOM INDEX EXCEPT GOAL1 & 10 (not just human freedom index also other variables :))

# Load the necessary library
library(readr)

# Count the number of NA values per column
na_count <- sapply(data_question1, function(x) sum(is.na(x)))

# Print the results
print(na_count) #105 NA FOR GOAL1&5

# goal1
question1_missing_goal1 <- data_question1 %>%
  group_by(code) %>%
  summarize(Na_goal1 = mean(is.na(goal1)))%>%
  filter(Na_goal1 != 0)

#we can see that for "KWT" "NZL" "OMN" "SGP" "UKR", there are 100% missing values for goal1
#we need to get rid of them
data_question1 <- data_question1 %>% filter(!code %in% question1_missing_goal1$code)

# Update List of countries deleted
list_country_deleted <- c(list_country_deleted, "KWT","NZL","OMN","SGP","UKR")

#still 42 NA values goal10

#goal10

question1_missing_goal10 <- data_question1 %>%
  group_by(code) %>%
  summarize(Na_goal10 = mean(is.na(goal10)))%>%
  filter(Na_goal10 != 0)

#100% missing for "GUY" "TTO" for goal10 -> get rid of these countries

data_question1 <- data_question1 %>% filter(!code %in% question1_missing_goal10$code)

# Update List of countries deleted
list_country_deleted <- c(list_country_deleted, "GUY","TTO")

#NO NA VALUES ANYMORE IN data_question1

#### Questions 2 and 4
see_missing24 <- data_question24 %>%
  group_by(code) %>%
  summarise(across(everything(), ~ sum(is.na(.))) %>%
              mutate(num_missing = rowSums(across(everything()))) %>%
              filter(num_missing > 0))

data_question24 <- data_question24 %>%
  group_by(country) %>%
  filter(!all(is.na(goal1)) & !all(is.na(goal10)))
# We remove the countries that have all NA values for goal1 and goal10

#### Question 3

# Disasters
variable_names <- names(data_question3_1)
missing_percentages <- sapply(data_question3_1, function(col) mean(is.na(col)) * 100)

missing_data_summary <- data.frame(
  Variable = variable_names,
  Missing_Percentage = missing_percentages
)

missing_data_summary <- missing_data_summary %>%
  mutate(VariableGroup = ifelse(startsWith(Variable, "goal") & Missing_Percentage == 0, "Goals without NAs", as.character(Variable)))

ggplot(data = missing_data_summary, aes(x = reorder(VariableGroup, Missing_Percentage), y = Missing_Percentage, fill = Missing_Percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Missing_Percentage > 1, sprintf("%.1f%%", Missing_Percentage), ""),
                y = Missing_Percentage),
            position = position_stack(vjust = 1),  # Adjust vertical position
            color = "white",  # Text color
            size = 3,          # Text size
            hjust = 1.05) +
  labs(title = "Percentage of Missing Values by Variable",
       x = "Variable",
       y = "Missing Percentage") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  coord_flip()

see_missing3_1 <- data_question3_1 %>%
  group_by(code) %>%
  summarise(across(-c(goal1, goal10),  # Exclude columns "goal1" and "goal10"
                   ~ sum(is.na(.))) %>%
              mutate(num_missing = rowSums(across(everything()))) %>%
              filter(num_missing > 0))
# Many missing, what do we do?

# Replace the missing values by zero
data_question3_1[is.na(data_question3_2)] <- 0

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

##### EXPORT as CSV #####

write.csv(data_question1, file = here("scripts","data","data_question1.csv"))
write.csv(data_question24, file = here("scripts","data","data_question24.csv"))
write.csv(data_question3_1, file = here("scripts","data","data_question3_1.csv"))
write.csv(data_question3_2, file = here("scripts","data","data_question3_2.csv"))
write.csv(data_question3_3, file = here("scripts","data","data_question3_3.csv"))
