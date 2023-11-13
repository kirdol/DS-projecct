# Load and prepare unemployment rate dataset
D2_1_Unemployment_rate <- read.csv(here("scripts", "data", "UnemploymentRate.csv")) %>%
  as.data.frame() %>%
  mutate(
    ref_area.label = iconv(ref_area.label, to = "UTF-8", sub = "byte"),
    ref_area.label = countrycode(ref_area.label, "country.name", "country.name"),
    time = as.integer(time),
    `unemployment rate` = obs_value / 100
  ) %>%
  filter(time >= 2000, time <= 2022) %>%
  select(-source.label, -obs_status.label, -indicator.label, -sex.label, -classif1.label) %>%
  rename(
    country = ref_area.label,
    year = time,
    `unemployment rate` = obs_value
  ) %>%
  inner_join(D1_0_SDG_country_list[, c("country", "code")], by = c("country" = "country")) %>%
  filter(!str_detect(sex.label, "Male|Female")) %>%
  select(code, country, year, `unemployment rate`)

# Filter to keep only unemployment rate for 15 years old and above.
D2_1_Unemployment_rate <- D2_1_Unemployment_rate %>%
  filter(`age category` == "Age (Youth, adults): 15+") %>%
  select(-`age category`)

# Identify missing countries
D2_1_Unemployment_rate_country_list <- distinct(select(D2_1_Unemployment_rate, code, country))
D_2_1_Unemployment_rate_missing_countries <- setdiff(D1_0_SDG_country_list$code, D2_1_Unemployment_rate_country_list$code)
print(D_2_1_Unemployment_rate_missing_countries)


# Missing countries analysis (assumed process)
list_country_Unemp <- unique(D2_1_Unemployment_rate_country_list$code)
missing <- setdiff(list_country, list_country_Unemp)
length(unique(D2_1_Unemployment_rate$code))