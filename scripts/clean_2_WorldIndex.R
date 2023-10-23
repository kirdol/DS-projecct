## 2 World Index
D2_1_Unemployment_rate <- # import the dataset
  read.csv(here("DS-project/scripts/data/UNE_2EAP_SEX_AGE_RT_A-full-2023-10-19.csv"))

D2_1_Unemployment_rate <- # make sure that we have a datafraame
  as.data.frame(D2_1_Unemployment_rate)

# Here, we use the dataset of the internet usage to extract the code of the countries.
D4_0_Internet_usage <- # We import the dataset of Internet Usage
  read.csv(here("DS-project/scripts/data/share-of-individuals-using-the-internet-2.csv"))

countries_codes <- D4_0_Internet_usage %>% # We create a new dataframe with the coutriy names and code.
  select(Entity, Code) %>%
  distinct()

D2_1_Unemployment_rate <- # We merge the dataset of Unemployment rate with codes and the dataset containing the codes for each country
  merge(D2_1_Unemployment_rate, countries_codes[, c("Entity", "Code")], by.x = "ref_area.label", by.y = "Entity", all.x = TRUE)

D2_1_Unemployment_rate <- # keep only the data after 2000 to match the main datast
  D2_1_Unemployment_rate[D2_1_Unemployment_rate$time >= 2000, ]

## Cost of living
D2_2_Cost_of_living # TBD

## Crime Index
D2_3_Crime_index # TBD
