GDPpercapita <-
  read.csv(here("scripts","data","GDPpercapita.csv"), sep = ";")
MilitaryExpenditurePercentGDP <-
  read.csv(here("scripts","data","MilitaryExpenditurePercentGDP.csv"), sep = ";")
MiliratyExpenditurePercentGovExp <-
  read.csv(here("scripts","data","MiliratyExpenditurePercentGovExp.csv"), sep = ";")

# fill in the missing country codes using the column Indicator.Name

fill_code <- function(data){
  data <- data %>%
    mutate(Country.Code = ifelse(!grepl("^[A-Z]{3}$", Country.Code), Indicator.Name, Country.Code))
}

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
  data <- fill_code(data)
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
# But we suspect that some of the missing countries were in the database but not rightly matched

list_country_GDP <- c(unique(GDPpercapita$code))
(missing <- setdiff(list_country, list_country_GDP))

# 1. Bahamas was in the database but instead of the code "BHS" there is "The"
# 2. "COD" "Dem. Rep."
# 3. "COG" "Rep"
# 4. "EGY" "Arab Rep."
# 5. "GMB" "The"
# 6. "IRN" "Islamic Rep."
# 7. "KOR" "Rep."
# 8. "VEN" "RB"
# 9. "YEM" "Rep."
# We remark that the code is in another column of the initial database: "Indicator.Name"
# We go back to the initial database and before cleaning it we put the right codes

# After rerunning the code we see that we have all our 166 countries from the initial dataset 

# What is the percentage of missing values in these 3 datasets?

mean(is.na(MiliratyExpenditurePercentGovExp$MiliratyExpenditurePercentGovExp))
mean(is.na(MilitaryExpenditurePercentGDP$MilitaryExpenditurePercentGDP))
mean(is.na(GDPpercapita$GDPpercapita))

# 16.4% for MiliratyExpenditurePercentGovExp, 12.9% for MilitaryExpenditurePercentGDP and 1.31% for GDPpercapita

####### Investigate missing values in GDPpercapita ######

GDPpercapita1 <- GDPpercapita %>%
  group_by(code) %>%
  summarize(NaGDP = round(mean(is.na(GDPpercapita)), 3)) %>%
  filter(NaGDP != 0)

print(GDPpercapita1)

# Only SOM and SSD have a lot of missings and in total 11 countries with missings

# Create a dataframe that only have the countries with missing values and 
# add a column which contains the % of missings for each country

filtered_data_GDP <- GDPpercapita %>%
  filter(code %in% GDPpercapita1$code)

filtered_data_GDP <- filtered_data_GDP %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(GDPpercapita))) %>%
  ungroup()

# Look at the evolution over the years for the countries that have missing values

Evol_Missing_GDP <- ggplot(data = filtered_data_GDP) +
  geom_point(aes(x = year, y = GDPpercapita, 
                 color = cut(PercentageMissing,
                             breaks = c(0, 0.1, 0.2, 0.3, 1),
                             labels = c("0-10%", "10-20%", "20-30%", "30-100%")))) +
  labs(title = "Scatter Plot of GDP per capita", x = "Year", y = "GDP per capita") +
  scale_color_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%" = "red", "30-100%" = "black"),
                     labels = c("0-10%", "10-20%", "20-30%", "50-100%")) +
  guides(color = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Evol_Missing_GDP)

# We decide not to use SSD SOM and VEN since there are more than 30% missing

####### Fill in missing values in GDPpercapita ######

# Almost all have a linear evolution over time, we fill in the missing values based on the lines

# AFG, BTN, CUB, STP and TKM are easy with only one line

list_code <- c("AFG", "BTN", "CUB", "STP", "TKM")

for (i in list_code) {
  # Filter the dataset for the current country
  country_data <- GDPpercapita %>% filter(code == i)
  
  # Perform linear interpolation for the current country's data
  interpolated_data <- na.interp(country_data$GDPpercapita)
  
  # Update the original dataset with the interpolated values
  GDPpercapita[GDPpercapita$code == i, "GDPpercapita"] <- interpolated_data
}

# SYR and YEM : we fit 2 lines to fill the values

# LBN: weird at the end, we don't fill the missing value for now

##### Investigate missing values in MilitaryExpenditurePercentGDP #####

MilitaryExpenditurePercentGDP1 <- MilitaryExpenditurePercentGDP %>%
  group_by(code) %>%
  summarize(NaMil1 = round(mean(is.na(MilitaryExpenditurePercentGDP)),3)) %>%
  filter(NaMil1 != 0)

print(table(MilitaryExpenditurePercentGDP1$NaMil1))

# 100% missing: a lot! 12 countries

# Create a dataframe that only have the coutnries with missing values and 
# add a column which contains the % of missings for each country

filtered_data_Mil1 <- MilitaryExpenditurePercentGDP %>%
  filter(code %in% MilitaryExpenditurePercentGDP1$code)

filtered_data_Mil1 <- filtered_data_Mil1 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(MilitaryExpenditurePercentGDP))) %>%
  ungroup()

# Look at evolution over the years

Evol_Missing_Mil1 <- ggplot(data = filtered_data_Mil1) +
  geom_point(aes(x = year, y = MilitaryExpenditurePercentGDP, 
                 color = cut(PercentageMissing,
                             breaks = c(0, 0.1, 0.2, 0.3, 1),
                             labels = c("0-10%", "10-20%", "20-30%", "30-100%")))) +
  labs(title = "Scatter Plot of military expenditure in % of GDP", x = "Year", y = "GDP per capita") +
  scale_color_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%" = "red", "30-100%" = "black"),
                     labels = c("0-10%", "10-20%", "20-30%", "50-100%")) +
  guides(color = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 5)

print(Evol_Missing_Mil1)

# Try to fill the missings if %missings < 30%

##### Fill in missing values in MilitaryExpenditurePercentGDP #####

# "AFG", "BDI", "BEN", "CAF", "CIV", "COD", "GAB", "GMB", "KAZ", "LBN", "LBR", "MNE", "MRT", "NER", "TKJ", "TTO", "ZMB"
# <30% missing and linear (17)

list_code <- c("AFG", "BDI", "BEN", "CAF", "CIV", "COD", "GAB", "GMB", "KAZ", "LBN", "LBR", "MNE", "MRT", "NER", "TKJ", "TTO", "ZMB")

for (i in list_code) {
  # Filter the dataset for the current country
  country_data <- MilitaryExpenditurePercentGDP %>% filter(code == i)
  
  # Perform linear interpolation for the current country's data
  interpolated_data <- na.interp(country_data$MilitaryExpenditurePercentGDP)
  
  # Update the original dataset with the interpolated values
  MilitaryExpenditurePercentGDP[MilitaryExpenditurePercentGDP$code == i, "MilitaryExpenditurePercentGDP"] <- interpolated_data
}

# "BIH", "COG", "IRQ", "MMR", "SDN", "TCD", "TGO", "ZWE"
# <30% missing but not linear (keep but we will see later) (8)

# Others have too much missing (24)

##### Investigate missing values in MilitaryExpenditurePercentGovExp #####

MiliratyExpenditurePercentGovExp1 <- MiliratyExpenditurePercentGovExp %>%
  group_by(code) %>%
  summarize(NaMil2 = round(mean(is.na(MiliratyExpenditurePercentGovExp)),3)) %>%
  filter(NaMil2 != 0)

print(table(MiliratyExpenditurePercentGovExp1$NaMil2))

# 100% missing: a lot ! 17 countries

# Create a dataframe that only have the coutnries with missing values and 
# add a column which contains the % of missings for each country

filtered_data_Mil2 <- MiliratyExpenditurePercentGovExp %>%
  filter(code %in% MiliratyExpenditurePercentGovExp1$code)

filtered_data_Mil2 <- filtered_data_Mil2 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(MiliratyExpenditurePercentGovExp))) %>%
  ungroup()

 # Look at evolution over the years

Evol_Missing_Mil2 <- ggplot(data = filtered_data_Mil2) +
  geom_point(aes(x = year, y = MiliratyExpenditurePercentGovExp, 
                 color = cut(PercentageMissing,
                             breaks = c(0, 0.1, 0.2, 0.3, 1),
                             labels = c("0-10%", "10-20%", "20-30%", "30-100%")))) +
  labs(title = "Scatter Plot of military expenditure in % of gov exp", x = "Year", y = "GDP per capita") +
  scale_color_manual(values = c("0-10%" = "blue", "10-20%" = "green", "20-30%" = "red", "30-100%" = "black"),
                     labels = c("0-10%", "10-20%", "20-30%", "50-100%")) +
  guides(color = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 5)

print(Evol_Missing_Mil2)

# Try to fill the missings if %missings < 30%

##### Fill in missing values in MilitaryExpenditurePercentGovExp #####

# "AFG", "ARM", BEN", "BIH", "BLR", COG", "ECU", GAB", "GMB", "KAZ", "LBN", "LBR", "MNE", "MWI", "NER", "TTO", "UKR", ZMB"
# <30% missing and linear (18)

list_code <- c("AFG", "ARM", "BEN", "BIH", "BLR", "COG", "ECU", "GAB", "GMB", "KAZ", "LBN", "LBR", "MNE", "MWI", "NER", "TTO", "UKR", "ZMB")

for (i in list_code) {
  # Filter the dataset for the current country
  country_data <- MiliratyExpenditurePercentGovExp %>% filter(code == i)
  
  # Perform linear interpolation for the current country's data
  interpolated_data <- na.interp(country_data$MiliratyExpenditurePercentGovExp)
  
  # Update the original dataset with the interpolated values
  MiliratyExpenditurePercentGovExp[MiliratyExpenditurePercentGovExp$code == i, "MiliratyExpenditurePercentGovExp"] <- interpolated_data
}

# "BDI", "IRQ"
# 2 lines (2)

# "CAF", "MMR", "SDN", "TCD", "TGO", "TJK" 
# <30% missing but not linear (keep but we will see later) (6)

# Others have too much missing (31) -> very much maybe we will have to drop this variable for our analysis

# And now, What is the percentage of missing values in these 3 datasets?

mean(is.na(MiliratyExpenditurePercentGovExp$MiliratyExpenditurePercentGovExp))
mean(is.na(MilitaryExpenditurePercentGDP$MilitaryExpenditurePercentGDP))
mean(is.na(GDPpercapita$GDPpercapita))

# Standardize names for merge
D3_1_GDP_per_capita <- GDPpercapita
D3_2_Military_Expenditure_Percent_GDP <- MilitaryExpenditurePercentGDP
D3_3_Miliraty_Expenditure_Percent_Gov_Exp <- MiliratyExpenditurePercentGovExp
