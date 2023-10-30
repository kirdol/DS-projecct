### 2 World Index
## Unemployment rate

D2_1_Unemployment_rate <- # import the dataset
  read.csv(here("scripts","data","UnemploymentRate.csv"))

D2_1_Unemployment_rate <- # make sure that we have a datafraame
  as.data.frame(D2_1_Unemployment_rate)

# Here, we use the dataset of the internet usage to extract the code of the countries.
D4_0_Internet_usage <- # We import the dataset of Internet Usage
  read.csv(here("scripts","data","InternetUsage.csv"))

countries_codes <- D4_0_Internet_usage %>% # We create a new dataframe with the coutriy names and code.
  select(Entity, Code) %>%
  distinct()

D2_1_Unemployment_rate <- # We merge the dataset of Unemployment rate with codes and the dataset containing the codes for each country
  merge(D2_1_Unemployment_rate, countries_codes[, c("Entity", "Code")], by.x = "ref_area.label", by.y = "Entity", all.x = TRUE)

D2_1_Unemployment_rate <- # keep only the data between 2000 and 2022 to match the main datast
  D2_1_Unemployment_rate[
    D2_1_Unemployment_rate$time >= 2000 & D2_1_Unemployment_rate$time <= 2022, ]

# remove colums that we do not need
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  select(-source.label, -obs_status.label, -indicator.label)

# rearrange the columns
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  select(Code, ref_area.label, time, sex.label, classif1.label, obs_value)

# rename columns
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  rename(
    "code" = Code,
    "country" = ref_area.label,
    "year" = time,
    "age category" = classif1.label,
    "unemployment rate" = obs_value
  )

# drop rows with sex indications and remove the column linked to sex
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  filter(!str_detect(sex.label, fixed("Male")) & !str_detect(sex.label, fixed("Female")))
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  select(-sex.label)

# changing to decimal values the unemployment rate
D2_1_Unemployment_rate$`unemployment rate` <-
  D2_1_Unemployment_rate$`unemployment rate` / 100


# cleaning of the environment
rm(D4_0_Internet_usage, countries_codes)

# We want to keep only the values for the country that appear in our main dataframe
D2_1_unique_code <- unique(D2_1_Unemployment_rate$code)

# Here we look at the country that are in the main dataframe but that are missing from the data on unemployment rate
missing_countries <- setdiff(D2_1_unique_code, list_country)
rm(D2_1_unique_code)
print(missing_countries)

# Here, we select only the countries that we want (specifies in "list_country")
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  filter(code %in% list_country)