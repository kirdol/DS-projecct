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

####### Investigate missing values in GDPpercapita ######

GDPpercapita1 <- GDPpercapita %>%
  group_by(code) %>%
  summarize(NaGDP = mean(is.na(GDPpercapita))) %>%
  filter(NaGDP != 0)
print(GDPpercapita1, n = 180)

# Only SOM and SSD have a lot of missings and in total 9 countries with missings

# Create a dataframe that only have the coutnries with missing values and 
# add a column which contains the % of missings for each country

filtered_data_GDP <- GDPpercapita %>%
  filter(code %in% GDPpercapita1$code)

filtered_data_GDP <- filtered_data_GDP %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(GDPpercapita))) %>%
  ungroup()

# Look at the distribution of values for the countries that have missing values

Freq_Missing_GDP <- ggplot(data = filtered_data_GDP) +
  geom_histogram(aes(x = GDPpercapita, 
                     fill = cut(PercentageMissing,
                                breaks = c(0, 0.25, 0.5, 1),
                                labels = c("0-24%", "25-49%", "50-100%"))),
                 bins = 30) +
  labs(title = "Histogram of GDP per capita", x = "GDP per capitaP", y = "Frequency") +
  scale_fill_manual(values = c("0-24%" = "blue", "25-49%" = "red", "50-100%" = "black"), labels = c("0-24%", "25-49%", "50-100%")) +
  guides(fill = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Freq_Missing_GDP)

# Look at the evolution over the years for the countries that have missing values

Evol_Missing_GDP <- ggplot(data = filtered_data_GDP) +
  geom_point(aes(x = year, y = GDPpercapita, 
                 color = cut(PercentageMissing,
                             breaks = c(0, 0.25, 0.5, 1),
                             labels = c("0-24%", "25-49%", "50-100%")))) +
  labs(title = "Scatter Plot of GDP per capita", x = "Year", y = "GDP per capita") +
  scale_color_manual(values = c("0-24%" = "blue", "25-49%" = "red", "50-100%" = "black"),
                     labels = c("0-24%", "25-49%", "50-100%")) +
  guides(color = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Evol_Missing_GDP)

####### Fill in missing values in GDPpercapita ######

##### Investigate missing values in MilitaryExpenditurePercentGDP #####

MilitaryExpenditurePercentGDP1 <- MilitaryExpenditurePercentGDP %>%
  group_by(code) %>%
  summarize(NaMil1 = mean(is.na(MilitaryExpenditurePercentGDP))) %>%
  filter(NaMil1 != 0)
print(MilitaryExpenditurePercentGDP1, n = 180)

# 100% missing: a lot! 11 countries

# Create a dataframe that only have the coutnries with missing values and 
# add a column which contains the % of missings for each country

filtered_data_Mil1 <- MilitaryExpenditurePercentGDP %>%
  filter(code %in% MilitaryExpenditurePercentGDP1$code)

filtered_data_Mil1 <- filtered_data_Mil1 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(MilitaryExpenditurePercentGDP))) %>%
  ungroup()

# Look at the distribution of values for the countries that have missing values

Freq_Missing_Mil1 <- ggplot(data = filtered_data_Mil1) +
  geom_histogram(aes(x = MilitaryExpenditurePercentGDP, 
                     fill = cut(PercentageMissing,
                                breaks = c(0, 0.25, 0.5, 1),
                                labels = c("0-24%", "25-49%", "50-100%"))),
                 bins = 30) +
  labs(title = "Histogram of Military exp in % of GDP", x = "Military exp in % of GDP", y = "Frequency") +
  scale_fill_manual(values = c("0-24%" = "blue", "25-49%" = "red", "50-100%" = "black"), labels = c("0-24%", "25-49%", "50-100%")) +
  guides(fill = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Freq_Missing_Mil1)

# Look at evolution over the years

Evol_Missing_Mil1 <- ggplot(data = filtered_data_Mil1) +
  geom_point(aes(x = year, y = MilitaryExpenditurePercentGDP, 
                 color = cut(PercentageMissing,
                             breaks = c(0, 0.25, 0.5, 1),
                             labels = c("0-24%", "25-49%", "50-100%")))) +
  labs(title = "Scatter Plot of Military exp in % of GDP", x = "Year", y = "Military exp in % of GDP") +
  scale_color_manual(values = c("0-24%" = "blue", "25-49%" = "red", "50-100%" = "black"),
                     labels = c("0-24%", "25-49%", "50-100%")) +
  guides(color = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Evol_Missing_Mil1)


# Conculsion: replace by mean when flat / replace by mean over 3 years before and after when 
# different tendencies / replace by value of the year before or after when suited

##### Fill in missing values in MilitaryExpenditurePercentGDP #####

##### Investigate missing values in MilitaryExpenditurePercentGovExp #####

MiliratyExpenditurePercentGovExp1 <- MiliratyExpenditurePercentGovExp %>%
  group_by(code) %>%
  summarize(NaMil2 = mean(is.na(MiliratyExpenditurePercentGovExp))) %>%
  filter(NaMil2 != 0)
print(MiliratyExpenditurePercentGovExp1, n = 180)

# 100% missing: a lot ! 14 countries

# Create a dataframe that only have the coutnries with missing values and 
# add a column which contains the % of missings for each country

filtered_data_Mil2 <- MiliratyExpenditurePercentGovExp %>%
  filter(code %in% MilitaryExpenditurePercentGDP1$code)

filtered_data_Mil2 <- filtered_data_Mil2 %>%
  group_by(code) %>%
  mutate(PercentageMissing = mean(is.na(MiliratyExpenditurePercentGovExp))) %>%
  ungroup()

# Look at the distribution of values for the countries that have missing values

Freq_Missing_Mil2 <- ggplot(data = filtered_data_Mil2) +
  geom_histogram(aes(x = MiliratyExpenditurePercentGovExp, 
                     fill = cut(PercentageMissing,
                                breaks = c(0, 0.25, 0.5, 1),
                                labels = c("0-24%", "25-49%", "50-100%"))),
                 bins = 30) +
  labs(title = "Histogram of Military exp in % of Gov Exp", x = "Military exp in % of Gov Exp", y = "Frequency") +
  scale_fill_manual(values = c("0-24%" = "blue", "25-49%" = "red", "50-100%" = "black"), labels = c("0-24%", "25-49%", "50-100%")) +
  guides(fill = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Freq_Missing_Mil2)

# Look at evolution over the years

Evol_Missing_Mil2 <- ggplot(data = filtered_data_Mil2) +
  geom_point(aes(x = year, y = MiliratyExpenditurePercentGovExp, 
                 color = cut(PercentageMissing,
                             breaks = c(0, 0.25, 0.5, 1),
                             labels = c("0-24%", "25-49%", "50-100%")))) +
  labs(title = "Scatter Plot of Military exp in % of Gov Exp", x = "Year", y = "Military exp in % of Gov Exp") +
  scale_color_manual(values = c("0-24%" = "blue", "25-49%" = "red", "50-100%" = "black"),
                     labels = c("0-24%", "25-49%", "50-100%")) +
  guides(color = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Evol_Missing_Mil2)

# Conclusion: replace by mean when flat / replace by mean over 3 years before and after when 
# different tendencies / replace by value of the year before or after when suited

##### Fill in missing values in MilitaryExpenditurePercentGovExp #####
