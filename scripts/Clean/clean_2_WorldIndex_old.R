### 2 World Index

## D2.1 Unemployment rate
D2_1_Unemployment_rate <- # import the dataset
  read.csv(here("scripts","data","UnemploymentRate.csv"))

D2_1_Unemployment_rate <- # make sure that we have a datafraame
  as.data.frame(D2_1_Unemployment_rate)

# Make sure the encoding of the country names are UTF-8
D2_1_Unemployment_rate$ref_area.label <- iconv(D2_1_Unemployment_rate$ref_area.label, to = "UTF-8", sub = "byte")

# standardize country names
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  mutate(ref_area.label = countrycode(ref_area.label, "country.name", "country.name"))

D2_1_Unemployment_rate <- # We merge the dataset of Unemployment rate with codes and the dataset containing the codes for each country
  merge(D2_1_Unemployment_rate, D1_0_SDG_country_list[, c("country", "code")], by.x = "ref_area.label", by.y = "country", all.x = TRUE)

D2_1_Unemployment_rate <- # keep only the data between 2000 and 2022 to match the main datast
  D2_1_Unemployment_rate[
    D2_1_Unemployment_rate$time >= 2000 & D2_1_Unemployment_rate$time <= 2022, ]

# remove colums that we do not need
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  select(-source.label, -obs_status.label, -indicator.label)

# rearrange the columns
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  select(code, ref_area.label, time, sex.label, classif1.label, obs_value)

# rename columns
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  rename(
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

# We want to keep only the values for the country that appear in our main dataframe
D2_1_Unemployment_rate_country_list <- unique(D2_1_Unemployment_rate$code)

# Here we look at the country that are in the main dataframe but that are missing from the data on unemployment rate
D2_1_Unemployment_rate_missing_countries <- setdiff(D1_0_SDG_country_list$code, D2_1_Unemployment_rate_country_list)
print(D_2_1_Unemployment_rate_missing_countries)

# Here, we select only the countries that we want (specified in "D1_0_SDG_country_list")
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  filter(code %in% D1_0_SDG_country_list$code)

D2_1_Unemployment_rate_country_list <- D2_1_Unemployment_rate %>%
  filter(code %in% D2_1_Unemployment_rate_country_list) %>%
  select(code, country)

D2_1_Unemployment_rate_country_list <- D2_1_Unemployment_rate_country_list %>%
  select(code, country) %>%
  distinct()

# Filter to keep only unemployment rate for 15 years old and above.
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  filter(`age category` == "Age (Youth, adults): 15+")

D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  select(-`age category`)

# Je tente un truc pour les pays manquants

list_country_Unemp <- c(unique(D2_1_Unemployment_rate_country_list$code))
(missing <- setdiff(list_country, list_country_Unemp))
(length(unique(D2_1_Unemployment_rate$code)))