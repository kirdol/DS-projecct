# Import data
D1_0_SDG <- read.csv(here("scripts","data","SDG.csv"), sep = ";")

# Transform -> dataframe

D1_0_SDG <- as.data.frame(D1_0_SDG)

# We only want to keep certain columns: country code, country, year, population, overall SDG score and the scores on each SDG

D1_0_SDG <- D1_0_SDG[,1:22]

# Rename the columns to have our variables

colnames(D1_0_SDG) <- c("code", "country", "year", "population", "overallscore", "goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal14", "goal15", "goal16", "goal17")

# Transform the SDG overall score into a numeric value

D1_0_SDG[["overallscore"]] <- as.double(gsub(",", ".", D1_0_SDG[["overallscore"]]))

# Function to transform the SDG score into numeric values

makenumSDG <- function(D1_0_SDG) {
  for (i in 1:17) {
    varname <- paste("goal", i, sep = "")
    D1_0_SDG[[varname]] <- as.double(gsub(",", ".", D1_0_SDG[[varname]]))
  }
  return(D1_0_SDG)
}

D1_0_SDG <- makenumSDG(D1_0_SDG)

# Make sure the encoding of the country names are UTF-8
D1_0_SDG$country <- stri_encode(D1_0_SDG$country, to = "UTF-8")


# standardize country names
D1_0_SDG <- D1_0_SDG %>%
  mutate(country = countrycode(country, "country.name", "country.name", custom_match = c("T�rkiye"="Turkey")))

# inspection of missing values

propmissing <- numeric(length(D1_0_SDG))

for (i in 1:length(D1_0_SDG)){
  proportion <- mean(is.na(D1_0_SDG[[i]]))
  propmissing[i] <- proportion
}
propmissing

# Population has some missing values, let's investigate

SDG0 <- D1_0_SDG |> 
  group_by(code) |> 
  select(population) |> 
  summarize(NaPop = mean(is.na(population))) |>
  filter(NaPop != 0)
print(SDG0, n = 180)

# Normal to have missing values because not countries but regions so we can drop these observations

D1_0_SDG <- D1_0_SDG %>%
  filter(!str_detect(code, "^_"))

# Now there isn't any more missing values in the variable population and we will see that we have information on 166 countries:

(country_number <- length(unique(D1_0_SDG$country)))

# Where do we have missing values in the different goal scores? 

SDG1 <- D1_0_SDG |> 
  group_by(code) |> 
  select(contains("goal")) |> 
  summarize(Na1 = mean(is.na(goal1)),
            Na2 = mean(is.na(goal2)),
            Na3 = mean(is.na(goal3)),
            Na4 = mean(is.na(goal4)),
            Na5 = mean(is.na(goal5)),
            Na6 = mean(is.na(goal6)),
            Na7 = mean(is.na(goal7)),
            Na8 = mean(is.na(goal8)),
            Na9 = mean(is.na(goal9)),
            Na10 = mean(is.na(goal10)),
            Na11 = mean(is.na(goal11)),
            Na12 = mean(is.na(goal12)),
            Na13 = mean(is.na(goal13)),
            Na14 = mean(is.na(goal14)),
            Na15 = mean(is.na(goal15)),
            Na16 = mean(is.na(goal16)),
            Na17 = mean(is.na(goal17))) |>
  filter(Na1 != 0 | Na2 != 0 | Na3 != 0| Na4 != 0| Na5 != 0| Na6 != 0| Na7 != 0| Na8 != 0| Na9 != 0| Na10 != 0| Na11 != 0| Na12 != 0| Na13 != 0| Na14 != 0| Na15 != 0| Na16 != 0| Na17 != 0)

# Print the counts for each variable
kable(for (col in names(SDG1)[-1]) {
  print(paste(col, "count:", sum(SDG1[[col]] != 0)))
})


# We that there are only missings in 3 SDG scores: 1, 10 and 14 and that when there are missings for a country, it is on all years or none. 

# More investigations of those 3 SDG scores
# A lot of countries don't have information on those 3 SDG, should we choose to not analyse these SDGs? 

SDG2 <- D1_0_SDG |> 
  group_by(code) |> 
  select(contains("goal")) |> 
  summarize(Na1 = mean(is.na(goal1))) |>
  filter(Na1 != 0)

print(table(SDG2$Na1))

length(unique(SDG2$code))/country_number

# there are only 9.04% missing values in 15 different countries, goal 1 being "end poverty", we 
# decide to keep it and only remove the countries with no information for the analysis

SDG3 <- D1_0_SDG |> 
  group_by(code) |> 
  select(contains("goal")) |> 
  summarize(Na10 = mean(is.na(goal10))) |>
  filter(Na10 != 0)

print(table(SDG3$Na10))

length(unique(SDG3$code))/country_number

# there are only 10.2% missing values in 17 different countries, goal 10 being "reduced inequalities", we 
# decide to keep it and only remove the countries with no information for the analysis

SDG4 <- D1_0_SDG |> 
  group_by(code) |> 
  select(contains("goal")) |> 
  summarize(Na14 = mean(is.na(goal14))) |>
  filter(Na14 != 0)

print(table(SDG4$Na14))

length(unique(SDG4$code))/country_number

# there are 24.1% missing values in 40 different countries, goal 14 being "life under water", we 
# decide not to keep it, because other SDG such as life on earth and clean water already treat similar subjects

# Delete SDG14

D1_0_SDG <- D1_0_SDG %>% select(-goal14)

# Standardize country code

D1_0_SDG$code <- countrycode(
  sourcevar = D1_0_SDG$code,
  origin = "iso3c",
  destination = "iso3c",
)

# Create a character vector with all the different country codes

list_country <- c(unique(D1_0_SDG$code))

# Create a dataframe with the list of countries and their respective codes

D1_0_SDG_country_list <- D1_0_SDG %>%
  filter(code %in% list_country) %>%
  select(code, country)

# remove duplicated rows
D1_0_SDG_country_list <- D1_0_SDG_country_list %>%
  select(code, country) %>%
  distinct()

# Complete database to make sure there aren't couples of (year, code) missing
D1_0_SDG <- D1_0_SDG |> complete(code, year)

