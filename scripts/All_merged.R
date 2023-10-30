
# pre-cleaning of the datasets
liste_de_scripts <- c("clean_1_SDG.R",
                      "clean_2_WorldIndex.R",
                      "clean_3_GDPmilitaryExp.R",
                      "clean_4_InternetUsage",
                      "clean_6_Disasters",
                      "clean_7_COVID",
                      "clean_9_Conflicts")

for (script in liste_de_scripts) {
  source(here("scripts", script))}
rm(liste_de_scripts)

merge_1_2 <- merge(D1_0_SDG,
                   D2_1_Unemployment_rate,
                   by = c("code", "year"), all.x = TRUE)

merge_12_3 <- merge(merge_1_2,
                    GDPpercapita,
                    by = c("code", "year"), all.x = TRUE)

# Also add MilitaryExpenditurePercentGDP and MiliratyExpenditurePercentGovExp :)

merge_123_4 <- merge(merge_12_3,
                     D4_0_Internet_usage,
                     by = c("code", "year"), all.x = TRUE)

merge_1234_5 <- merge(merge_123_4,
                      ???,
                      by = c("code", "year"), all.x = TRUE)

merge_12345_6 <- merge(merge_1234_5,
                       D_6_0_Disasters,
                       by = c("code", "year"), all.x = TRUE)

merge_123456_7 <- merge(merge_12345_6,
                        COVID,
                        by = c("code", "year"), all.x = TRUE)

merge_1234567_8 <- merge(merge_123456_7,
                         ???,
                         by = c("code", "year"), all.x = TRUE)

All_merged<- merge(merge_1234567_8, Conflicts, by = c("code", "year"), all.x = TRUE)

# cleaning of the environment
rm(merge_1_2,
   merge_12_3,
   merge_123_4,
   merge_1234_5,
   merge_12345_6,
   merge_123456_7,
   merge_1234567_8)