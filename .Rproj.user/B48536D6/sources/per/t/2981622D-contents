# Load and transform the dataset
D4_0_Internet_usage <- read.csv(here("scripts", "data", "InternetUsage.csv")) %>%
  filter(Year >= 2000, Year <= 2022) %>%
  rename(
    code = Code,
    country = Entity,
    year = Year,
    internet_usage = Individuals.using.the.Internet....of.population.
  ) %>%
  mutate(internet_usage = internet_usage / 100) %>%
  filter(code %in% list_country) %>%
  select(code, country, year, internet_usage) %>%
  arrange(code, year)
