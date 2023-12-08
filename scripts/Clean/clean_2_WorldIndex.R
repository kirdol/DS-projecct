# Load and preprocess the dataset
D2_1_Unemployment_rate <- read.csv(here("scripts","data","UnemploymentRate.csv")) %>%
  as.data.frame() %>%
  mutate(
    country = iconv(ref_area.label, to = "UTF-8", sub = "byte"),
    country = countrycode(country, "country.name", "country.name"),
    year = time,
    `unemployment rate` = obs_value / 100,
    age_category = classif1.label,
    sex = sex.label
  ) %>%
  select(-ref_area.label, -time, -obs_value, -classif1.label, -sex.label, -source.label, -obs_status.lMASSabel, -indicator.label) %>%
  merge(D1_0_SDG_country_list[, c("country", "code")], by = "country", all.x = TRUE) %>%
  filter(year >= 2000 & year <= 2022,
         !str_detect(sex, fixed("Male")) & !str_detect(sex, fixed("Female")),
         code %in% D1_0_SDG_country_list$code,
         age_category == "Age (Youth, adults): 15+") %>%
  select(code, country, year, `unemployment rate`) %>%
  distinct()

# Handling missing countries
D2_1_Unemployment_rate_missing_countries <- setdiff(D1_0_SDG_country_list$code, unique(D2_1_Unemployment_rate$code))
print(D2_1_Unemployment_rate_missing_countries)