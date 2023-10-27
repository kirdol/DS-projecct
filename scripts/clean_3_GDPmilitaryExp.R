GDPpercapita <-
  read.csv(here("scripts","data","GDPpercapita.csv"), sep = ";")
MilitaryExpenditurePercentGDP <-
  read.csv(here("scripts","data","MilitaryExpenditurePercentGDP.csv"), sep = ";")
MiliratyExpenditurePercentGovExp <-
  read.csv(here("scripts","data","MiliratyExpenditurePercentGovExp.csv"), sep = ";")

# remove the variables that we don't need

remove <- function(data){
  years <- seq(1960, 1999)
  removeyears <- paste("X", years, sep = "")
  data <- data[, !(names(data) %in% c("Indicator.Name", "Indicator.Code", "X", removeyears))]
}

# Make sure that the values are numeric

makenum <- function(data) {
  for (i in 2000:2022) {
    year <- paste("X", i, sep = "")
    data[[year]] <- as.numeric(data[[year]])
  }
  return(data)
}

# Rename years from Xyear 

renameyear <- function(data) {
  for (i in 2000:2022) {
    varname <- paste("X", i, sep = "")
    names(data)[names(data) == varname] <- gsub("X", "", varname)
  }
  return(data)
}

# Transform the database from wide to long

wide2long <- function(data) {
  data <- pivot_longer(data, 
                       cols = -c("Country.Name", "Country.Code"), 
                       names_to = "year", 
                       values_to = "data")
  return(data)
}

# Transform the year variable into an integer variable

yearint <- function(data) {
  data$year <- as.integer(data$year)
  return(data)
}

# rearrange and rename the columns to match the ones of the other datasets

nameorder <- function(data) {
  colnames(data) <- c("country", "code", "year", "data")
  data <- data %>% select(c("code", "country", "year", "data"))
}

# one function that contains all the others

cleanwide2long <- function(data){
  data <- remove(data)
  data <- makenum(data)
  data <- renameyear(data)
  data <- wide2long(data)
  data <- yearint(data)
  data <- nameorder(data)
}

# Apply function to three database

GDPpercapita <- cleanwide2long(GDPpercapita)
MilitaryExpenditurePercentGDP <- cleanwide2long(MilitaryExpenditurePercentGDP)
MiliratyExpenditurePercentGovExp <- cleanwide2long(MiliratyExpenditurePercentGovExp)

# Rename the data columns to have the right name

GDPpercapita <- GDPpercapita %>%
  rename(GDPpercapita = data)

MilitaryExpenditurePercentGDP <- MilitaryExpenditurePercentGDP %>%
  rename(MilitaryExpenditurePercentGDP = data)

MiliratyExpenditurePercentGovExp <- MiliratyExpenditurePercentGovExp %>%
  rename(MiliratyExpenditurePercentGovExp = data)

# Standardize the country code

GDPpercapita$code <- countrycode(
  sourcevar = GDPpercapita$code,
  origin = "iso3c",
  destination = "iso3c",
)

MilitaryExpenditurePercentGDP$code <- countrycode(
  sourcevar = MilitaryExpenditurePercentGDP$code,
  origin = "iso3c",
  destination = "iso3c",
)

MiliratyExpenditurePercentGovExp$code <- countrycode(
  sourcevar = MiliratyExpenditurePercentGovExp$code,
  origin = "iso3c",
  destination = "iso3c",
)

# Remove the obervations of countries that aren't in our main dataset on SDGs: 

GDPpercapita <- GDPpercapita %>% filter(code %in% list_country)
length(unique(GDPpercapita$code))

MilitaryExpenditurePercentGDP <- MilitaryExpenditurePercentGDP %>% filter(code %in% list_country)
length(unique(MilitaryExpenditurePercentGDP$code))

MiliratyExpenditurePercentGovExp <- MiliratyExpenditurePercentGovExp %>% filter(code %in% list_country)
length(unique(MiliratyExpenditurePercentGovExp$code))

# There are only 157 countries that are both in the main SDG dataset and in these 3 datasets

# What is the percentage of missing values in these 3 datasets?

mean(is.na(MiliratyExpenditurePercentGovExp$MiliratyExpenditurePercentGovExp))
mean(is.na(MilitaryExpenditurePercentGDP$MilitaryExpenditurePercentGDP))
mean(is.na(GDPpercapita$GDPpercapita))

# 15% for MiliratyExpenditurePercentGovExp, 12.5% for MilitaryExpenditurePercentGDP and 1.11% for GDPpercapita

####### Investigate missing values in GDPpercapita #####

GDPpercapita1 <- GDPpercapita %>%
  group_by(code) %>%
  summarize(NaGDP = mean(is.na(GDPpercapita))) %>%
  filter(NaGDP != 0)
print(GDPpercapita1, n = 180)

# Only SOM and SSD have a lot of missings and in total 9 countries with missings

# What happens if we only take the years from 2005?

GDPpercapita2 <- GDPpercapita %>% select(GDPpercapita$year>=2005)

GDPpercapita3 <- GDPpercapita2 %>%
  group_by(code) %>%
  summarize(NaGDP = mean(is.na(GDPpercapita2))) %>%
  filter(NaGDP != 0)
print(GDPpercapita3, n = 180)

# AFG -> could replace the values with those of the previous years
plot(
  x = GDPpercapita$year[GDPpercapita$code == "AFG"],
  y = GDPpercapita$GDPpercapita[GDPpercapita$code == "AFG"],
  xlab = "Year",
  ylab = "GDP per Capita",
  main = "GDP per Capita for AFG"
)

# BTN: quite stable in the last years, replace missing value in 2022 with the mean of the 5 last years
plot(
  x = GDPpercapita$year[GDPpercapita$code == "BTN"],
  y = GDPpercapita$GDPpercapita[GDPpercapita$code == "BTN"],
  xlab = "Year",
  ylab = "GDP per Capita",
  main = "GDP per Capita for BTN"
)

# CUB: increasing, what do we do ?
plot(
  x = GDPpercapita$year[GDPpercapita$code == "CUB"],
  y = GDPpercapita$GDPpercapita[GDPpercapita$code == "CUB"],
  xlab = "Year",
  ylab = "GDP per Capita",
  main = "GDP per Capita for CUB"
)

# LBN: big drop, but could use the value of 2021 to fill in the missing value of 2022
plot(
  x = GDPpercapita$year[GDPpercapita$code == "LBN"],
  y = GDPpercapita$GDPpercapita[GDPpercapita$code == "LBN"],
  xlab = "Year",
  ylab = "GDP per Capita",
  main = "GDP per Capita for LBN"
)

# SOM: we don't have data before 2013, we decide not to include SOM when using GDPpercapita
plot(
  x = GDPpercapita$year[GDPpercapita$code == "SOM"],
  y = GDPpercapita$GDPpercapita[GDPpercapita$code == "SOM"],
  xlab = "Year",
  ylab = "GDP per Capita",
  main = "GDP per Capita for SOM"
)

# SSD: We only have some values between 2008-15, we decide not to include this country when using GDPpercapita
plot(
  x = GDPpercapita$year[GDPpercapita$code == "SSD"],
  y = GDPpercapita$GDPpercapita[GDPpercapita$code == "SSD"],
  xlab = "Year",
  ylab = "GDP per Capita",
  main = "GDP per Capita for SSD"
)

#STP: only one missing value that we can replace with the one from the previous year or use the prediction of a linear regression 
plot(
  x = GDPpercapita$year[GDPpercapita$code == "STP"],
  y = GDPpercapita$GDPpercapita[GDPpercapita$code == "STP"],
  xlab = "Year",
  ylab = "GDP per Capita",
  main = "GDP per Capita for STP"
)

#SYR: stable during the last years, we use the mean fôf the last 5 years to fill in the 2 missing values (2021-22)
plot(
  x = GDPpercapita$year[GDPpercapita$code == "SYR"],
  y = GDPpercapita$GDPpercapita[GDPpercapita$code == "SYR"],
  xlab = "Year",
  ylab = "GDP per Capita",
  main = "GDP per Capita for SYR"
)

# TKM: increases in a linear wya during the last five years -> use linear reg to fill in the 2 missing values
plot(
  x = GDPpercapita$year[GDPpercapita$code == "TKM"],
  y = GDPpercapita$GDPpercapita[GDPpercapita$code == "TKM"],
  xlab = "Year",
  ylab = "GDP per Capita",
  main = "GDP per Capita for TKM"
)

##### Investigate missing values in MilitaryExpenditurePercentGDP #####

MilitaryExpenditurePercentGDP1 <- MilitaryExpenditurePercentGDP %>%
  group_by(code) %>%
  summarize(NaGDP = mean(is.na(MilitaryExpenditurePercentGDP))) %>%
  filter(NaGDP != 0)
print(MilitaryExpenditurePercentGDP1, n = 180)

# 100% missing: a lot !11 countries

plot(
  x = MilitaryExpenditurePercentGDP$year[MilitaryExpenditurePercentGDP$code == "AFG"],
  y = MilitaryExpenditurePercentGDP$MilitaryExpenditurePercentGDP[MilitaryExpenditurePercentGDP$code == "AFG"],
  xlab = "Year",
  ylab = "MilitaryExpenditurePercentGDP",
  main = "MilitaryExpenditurePercentGDP for AFG"
)

##### Investigate missing values in MilitaryExpenditurePercentGDP #####

MiliratyExpenditurePercentGovExp1 <- MiliratyExpenditurePercentGovExp %>%
  group_by(code) %>%
  summarize(NaGDP = mean(is.na(MiliratyExpenditurePercentGovExp))) %>%
  filter(NaGDP != 0)
print(MiliratyExpenditurePercentGovExp1, n = 180)

# 100% missing: a lot ! 14 countries

plot(
  x = MiliratyExpenditurePercentGovExp$year[MiliratyExpenditurePercentGovExp$code == "AFG"],
  y = MiliratyExpenditurePercentGovExp$MiliratyExpenditurePercentGovExp[MiliratyExpenditurePercentGovExp$code == "AFG"],
  xlab = "Year",
  ylab = "MiliratyExpenditurePercentGovExp",
  main = "MiliratyExpenditurePercentGovExp for AFG"
)


