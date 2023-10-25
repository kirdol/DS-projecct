# Import data

library(here)
library(dplyr)
library(stringr)

SDG <- read.csv(here("scripts","data","SDG.csv"), sep = ";")

# Transform -> dataframe

SDG <- as.data.frame(SDG)

# We only want to keep certain columns: country code, country, year, population, overall SDG score and the scores on each SDG

SDG <- SDG[,1:22]

# Rename the columns to have our variables

colnames(SDG) <- c("code", "country", "year", "population", "overallscore", "goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal14", "goal15", "goal16", "goal17")

# Transform the SDG overall score into a numeric value

SDG[["overallscore"]] <- as.double(gsub(",", ".", SDG[["overallscore"]]))

# Function to transform the SDG score into numeric values

makenumSDG <- function(SDG) {
  for (i in 1:17) {
    varname <- paste("goal", i, sep = "")
    SDG[[varname]] <- as.double(gsub(",", ".", SDG[[varname]]))
  }
  return(SDG)
}

SDG <- makenumSDG(SDG)

# inspection of missing values

propmissing <- numeric(length(SDG))

for (i in 1:length(SDG)){
  proportion <- mean(is.na(SDG[[i]]))
  propmissing[i] <- proportion
}
propmissing

# Where do we have missing values in the different goals? 

SDG1 <- SDG |> 
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

print(SDG1, n = 180)

# We that there are only missings in 3 SDG scores: 1, 10 and 14 and that when there are missings for a country, it is on all years or none. 

# More investigations of those 3 SDG scores

SDG2 <- SDG |> 
  group_by(code) |> 
  select(contains("goal")) |> 
  summarize(Na1 = mean(is.na(goal1)),
            Na10 = mean(is.na(goal10)),
            Na14 = mean(is.na(goal14))) |>
  filter(Na1 != 0 | Na10 != 0 | Na14 != 0)
print(SDG2, n = 180)

# A lot of countries don't have information on those 3 SDG, should we choose to not analyse these SDGs? -> enlever 14 les autre environ 10% missing

# Population has also some missing values, let's investigate

SDG3 <- SDG |> 
  group_by(code) |> 
  select(population) |> 
  summarize(NaPop = mean(is.na(population))) |>
  filter(NaPop != 0)
print(SDG3, n = 180)

# Normal to have missing values because not countries but regions so we can drop these observations

SDG <- SDG %>%
  filter(!str_detect(code, "^_"))

# Now there isn't any more missing values in the variable population and we will see that we have information on 166 countries:

length(unique(SDG$country))

