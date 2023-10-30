
# Pre-cleaning of the datasets
liste_de_scripts <- c("setup.R", # list of all the scripts needed to clean all individual dataset
                      "clean_1_SDG.R",
                      "clean_2_WorldIndex.R",
                      "clean_3_GDPmilitaryExp.R",
                      "clean_4_InternetUsage",
                      "clean_6_Disasters",
                      "clean_7_COVID",
                      "clean_9_Conflicts")

for (script in liste_de_scripts) { # execute each sript
  source(here("scripts", script))}

# merge D1_0_SDG with D2_1_Unemployment_rate 
merge_1_2 <- merge(D1_0_SDG,
                   D2_1_Unemployment_rate,
                   by = c("code", "year"), all.x = TRUE)

# merge merge_1_2 with GDPpercapita 
merge_12_3 <- merge(merge_1_2,
                    GDPpercapita,
                    by = c("code", "year"), all.x = TRUE)

# Also add MilitaryExpenditurePercentGDP and MiliratyExpenditurePercentGovExp :)

# merge merge_12_3 with D4_0_Internet_usage 
merge_123_4 <- merge(merge_12_3,
                     D4_0_Internet_usage,
                     by = c("code", "year"), all.x = TRUE)

# merge merge_123_4 with ??? 
merge_1234_5 <- merge(merge_123_4,
                      ???,
                      by = c("code", "year"), all.x = TRUE)

# merge merge_1234_5 with D_6_0_Disasters
merge_12345_6 <- merge(merge_1234_5,
                       D_6_0_Disasters,
                       by = c("code", "year"), all.x = TRUE)

# merge merge_12345_6 with COVID
merge_123456_7 <- merge(merge_12345_6,
                        COVID,
                        by = c("code", "year"), all.x = TRUE)

# merge merge_123456_7 with ???
merge_1234567_8 <- merge(merge_123456_7,
                         ???,
                         by = c("code", "year"), all.x = TRUE)

# merge merge_1234567_8 with Conflicts
All_merged<- merge(merge_1234567_8,
                   Conflicts,
                   by = c("code", "year"), all.x = TRUE)

# cleaning of the environment
rm(merge_1_2, # remove merge_1_2 from memory
   merge_12_3, # remove merge_12_3 from memory
   merge_123_4, # remove merge_123_4 from memory
   merge_1234_5, # remove merge_1234_5 from memory
   merge_12345_6, # remove merge_12345_6 from memory
   merge_123456_7, # remove merge_123456_7 from memory
   merge_1234567_8, # remove merge_1234567_8 from memory
   liste_de_scripts) # remove the list of scripts from memory)