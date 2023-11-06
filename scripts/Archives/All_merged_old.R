library(here)
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
merge_1_2 <- merge(D1_0_SDG,
                   D2_1_Unemployment_rate,
                   by = c("code", "year"), all.x = TRUE)
merge_1_2 <- merge_1_2 %>%
  select(-country.y)
merge_1_2 <- merge_1_2 %>%
  rename("country" = "country.x")

# merge merge_1_2 with D3_x_GDP_per_capita
merge_12_3 <- merge(merge_1_2,
                    D3_1_GDP_per_capita,
                    by = c("code", "year"), all.x = TRUE)
merge_12_3 <- merge_12_3 %>%
  select(-country.y)
merge_12_3 <- merge_12_3 %>%
  rename("country" = "country.x")

merge_12_3 <- merge(merge_1_2,
                    D3_2_Military_Expenditure_Percent_GDP,
                    by = c("code", "year"), all.x = TRUE)
merge_12_3 <- merge_12_3 %>%
  select(-country.y)
merge_12_3 <- merge_12_3 %>%
  rename("country" = "country.x")

merge_12_3 <- merge(merge_1_2,
                    D3_3_Miliraty_Expenditure_Percent_Gov_Exp,
                    by = c("code", "year"), all.x = TRUE)
merge_12_3 <- merge_12_3 %>%
  select(-country.y)
merge_12_3 <- merge_12_3 %>%
  rename("country" = "country.x")

# Also add MilitaryExpenditurePercentGDP and MiliratyExpenditurePercentGovExp :)

# merge merge_12_3 with D4_0_Internet_usage 
merge_123_4 <- merge(merge_12_3,
                     D4_0_Internet_usage,
                     by = c("code", "year"), all.x = TRUE)
merge_123_4 <- merge_123_4 %>%
  select(-country.y)
merge_123_4 <- merge_123_4 %>%
  rename("country" = "country.x")

# merge merge_123_4 with D5_0_Human_freedom_index
merge_1234_5 <- merge(merge_123_4,
                      D5_0_Human_freedom_index,
                      by = c("code", "year"), all.x = TRUE)
merge_1234_5 <- merge_1234_5 %>%
  select(-country.y)
merge_1234_5 <- merge_1234_5 %>%
  rename("country" = "country.x")

# merge merge_1234_5 with D_6_0_Disasters
merge_12345_6 <- merge(merge_1234_5,
                       D_6_0_Disasters,
                       by = c("code", "year"), all.x = TRUE)
merge_12345_6 <- merge_12345_6 %>%
  select(-country.y)
merge_12345_6 <- merge_12345_6 %>%
  rename("country" = "country.x")

# merge merge_12345_6 with D7_0_COVID
merge_123456_7 <- merge(merge_12345_6,
                        D7_0_COVID,
                        by = c("code", "year"), all.x = TRUE)
merge_123456_7 <- merge_123456_7 %>%
  select(-country.y)
merge_123456_7 <- merge_123456_7 %>%
  rename("country" = "country.x")

# merge merge_123456_7 with D8_0_Conflicts
All_Merged<- merge(merge_123456_7,
                   D8_0_Conflicts,
                   by = c("code", "year"), all.x = TRUE)
All_Merged <- All_Merged %>%
  select(-country.y)
All_Merged <- All_Merged %>%
  rename("country" = "country.x")

# cleaning of the environment
rm(merge_1_2, # remove merge_1_2 from memory
   merge_12_3, # remove merge_12_3 from memory
   merge_123_4, # remove merge_123_4 from memory
   merge_1234_5, # remove merge_1234_5 from memory
   merge_12345_6, # remove merge_12345_6 from memory
   merge_123456_7, # remove merge_123456_7 from memory
   merge_1234567_8, # remove merge_1234567_8 from memory
   liste_de_scripts) # remove the list of scripts from memory)
