### Importation

# libraries

library(here)

### 1 Main Table
SDG <- read.csv(here("scripts/data/SDG.csv"), sep = ";")

### 2 World Index
D2_1_Unemployment_rate <- read.csv(here("DS-project/scripts/data/UNE_2EAP_SEX_AGE_RT_A-full-2023-10-19.csv"))
D2_2_Cost_of_living # TBF
D2_3_Crime_index # TBF

# 3 World bank data on GDP population and military

# 4 Internet Usage
D4_0_Internet_usage <- read.csv(here("DS-project/scripts/data/share-of-individuals-using-the-internet-2.csv"))

# 5 The Human Freedom Index
hello

# 6 Natural Disasters

# 7 COVID-19 Pandemic Data Variables

# 8 Financial crisis information and many more

# 9 List of countries in conflicts from 1999 to 2019

# Modification
countries_codes <- D4_0_Internet_usage %>%
  select(Entity, Code) %>%
  distinct()
D4_0_Internet_usage_with_country_code <-
  merge(D2_1_Unemployment_rate, countries_codes[, c("Entity", "Code")], by.x = "ref_area.label", by.y = "Entity", all.x = TRUE)
