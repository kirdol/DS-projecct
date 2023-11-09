
## 4 Internet Usage
D4_0_Internet_usage <- # import the dataset
  read.csv(here("scripts","data","InternetUsage.csv"))

D4_0_Internet_usage <- # make sure that we have a datafraame
  as.data.frame(D4_0_Internet_usage)

D4_0_Internet_usage <- # keep only the data between 2000 and 2022 to match the main dataset
  D4_0_Internet_usage[
    D4_0_Internet_usage$Year >= 2000 & D4_0_Internet_usage$Year <= 2022 , ]

# renaming the columns for clarity
D4_0_Internet_usage <- D4_0_Internet_usage %>%
  rename(
    "code" = Code,
    "country" = Entity,
    "year" = Year,
    "internet usage" = Individuals.using.the.Internet....of.population.
  )

# change to decimals internet usage
D4_0_Internet_usage$`internet usage` <-
  D4_0_Internet_usage$`internet usage` / 100

# Keep only the countries that are in our main dataset
D4_0_Internet_usage <- D4_0_Internet_usage %>% filter(code %in% list_country)
length(unique(D4_0_Internet_usage$code))
