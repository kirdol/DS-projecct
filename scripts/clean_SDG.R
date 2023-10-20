SDG <- read.csv('C:/Users/Delia/Documents/1. Formations études école/HEC/MSc1/Data Science in Business Analytics/DS-projecct/SDG.csv', sep=';')

# Import data

SDG <- read.csv(here::here('SDG.csv'), sep=';')

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

# other

length(unique(SDG$country))
