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
merge_12_4 <- merge_12_3 |> left_join(D4_0_Internet_usage, join_by(code, year)) 

# merge merge_123_4 with D5_0_Human_freedom_index
D5_0_Human_freedom_index$country <- NULL
merge_12_5 <- merge_12_4 |> left_join(D5_0_Human_freedom_index, join_by(code, year)) 

# merge merge_1234_5 with D_6_0_Disasters
D_6_0_Disasters$country <- NULL
merge_12_6 <- merge_12_5 |> left_join(D_6_0_Disasters, join_by(code, year)) 

# merge merge_12345_6 with D7_0_COVID
D7_0_COVID$country <- NULL
D7_0_COVID <- D7_0_COVID |> distinct(code, year, .keep_all = TRUE)
merge_12_7 <- merge_12_6 |> left_join(D7_0_COVID, join_by(code, year)) 

# merge merge_123456_7 with D8_0_Conflicts
D8_0_Conflicts$country <- NULL
all_merge <- merge_12_7 |> left_join(D8_0_Conflicts, join_by(code, year)) 


# cleaning of the environment
rm(merge_1_2, # remove merge_1_2 from memory
   merge_12_3, # remove merge_12_3 from memory
   merge_123_4, # remove merge_123_4 from memory
   merge_1234_5, # remove merge_1234_5 from memory
   merge_12345_6, # remove merge_12345_6 from memory
   merge_123456_7, # remove merge_123456_7 from memory
   merge_1234567_8, # remove merge_1234567_8 from memory
   liste_de_scripts) # remove the list of scripts from memory)
