###Is the evolution in sustainable development influenced by uncontrollable events, such as economic crisis, health crises and natural disasters? ###

install.packages("corrplot")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("here")
library(corrplot)
library(ggplot2)
library(reshape2)
library(here)


# Importation of the data for this question
Q3.1 <- read.csv(here("scripts", "data", "data_question3_1.csv"))
Q3.2 <- read.csv(here("scripts", "data", "data_question3_2.csv"))
Q3.3 <- read.csv(here("scripts", "data", "data_question3_3.csv"))


#_____________________________________________________________________

#2. Data exploration
#Correlation_matrix <- cor(Q3.1[, c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goa10", "goal11", "goal12", "goal13", "goal15", "goal16", "total_deaths")])


# Convert 'year' column to date format if it's not already in date format
Q3.1$year <- as.Date(as.character(Q3.1$year), format = "%Y")
Q3.2$year <- as.Date(as.character(Q3.2$year), format = "%Y")
Q3.3$year <- as.Date(as.character(Q3.3$year), format = "%Y")

# Replace the missing values by zero
Q3.1[is.na(Q3.1)] <- 0
print(Q3.1)


##COVID***

# Time-series analysis of COVID-19 cases per million by region
# Filter COVID-19 data for dates after 2019
covid_filtered <- Q3.2[Q3.2$year >= as.Date("2019-01-01"), ]

ggplot(data = covid_filtered, aes(x = year, y = cases_per_million)) +
  geom_line() +
  labs(title = "Trend of COVID-19 Cases per Million Over Time", x = "Year", y = "Cases per Million")+
  facet_wrap(~ region, nrow = 2)  

#Nicer------------->
# Filtering the data for the relevant time period
covid_filtered <- Q3.2[Q3.2$year >= as.Date("2018-12-12"), ]

# Creating the ggplot with geom_smooth for each year
ggplot(data = covid_filtered, aes(x = year, y = cases_per_million, group = region, color = region)) +
  geom_smooth(method = "loess",  se = FALSE, span = 0.8, size = 0.5) + 
  labs(title = "Trend of COVID-19 Cases per Million Over Time",
       x = "Year", y = "Cases per Million") +
  facet_wrap(~ region, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")  # Using a different color palette for lines



#___-----____-----____-----
# Time-series analysis of COVID-19 deaths per million by region
ggplot(data = covid_filtered, aes(x = year, y = deaths_per_million)) +
  geom_line() +
  labs(title = "Trend of COVID-19 Deaths per Million Over Time", x = "Year", y = "Deaths per Million")+
  facet_wrap(~ region, nrow = 2)  
#Nicer ----->
# Creating the ggplot with geom_smooth for each year
ggplot(data = covid_filtered, aes(x = year, y = deaths_per_million, group = region, color = region)) +
  geom_smooth(method = "loess",  se = FALSE, span = 0.8, size = 0.5) + 
  labs(title = "Trend of COVID-19 Deaths per Million Over Time", x = "Year", y = "Deaths per Million") +
  facet_wrap(~ region, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")  # Using a different color palette for lines

#____----____----____----____----
# Time-series analysis of COVID-19 stringency by region
ggplot(data = covid_filtered, aes(x = year, y = stringency)) +
  geom_line() +
  labs(title = "Trend of COVID-19 Stringency Over Time", x = "Year", y = "Stringency")+
  facet_wrap(~ region, nrow = 2)  
#Nicer ------>
# Creating the ggplot with geom_smooth for each year
ggplot(data = covid_filtered, aes(x = year, y = stringency, group = region, color = region)) +
  geom_smooth(method = "loess",  se = FALSE, span = 0.7, size = 0.5) + 
  labs(title = "Trend of COVID-19 Stringency Over Time", x = "Year", y = "Stringency") +
  facet_wrap(~ region, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")  # Using a different color palette for lines


###DISASTERS***

# Time-series analysis of climatic disasters with total affected
ggplot(data = Q3.1, aes(x = year, y = total_affected)) +
  geom_line() +
  labs(title = "Trend of Total Affected from Climatic Disasters Over Time", x = "Year", y = "Total Affected")+
  facet_wrap(~ region, nrow = 2)  # Modify nrow as per your preference for rows in facet grid

#Nicer -------->
# Creating the ggplot with geom_smooth for each year
ggplot(data = Q3.1, aes(x = year, y = total_affected, group = region, color = region)) +
  geom_smooth(method = "loess",  se = FALSE, span = 0.7, size = 0.5) + 
  labs(title = "Trend of Total Affected from Climatic Disasters Over Time", x = "Year", y = "Total Affected") +
  facet_wrap(~ region, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")  # Using a different color palette for lines


###CONFLICTS***

# Time-series analysis of Conflicts
# Filter conflict data for the years between 2000 and 2016
conflicts_filtered <- Q3.3[Q3.3$year >= as.Date("2000-01-01") & Q3.3$year <= as.Date("2016-12-31"), ]

ggplot(data = conflicts_filtered, aes(x = year, y = sum_deaths)) +
  geom_line() +
  labs(title = "Trend of Deaths by Conflicts Over Time", x = "Year", y = "Sum Deaths")+
  facet_wrap(~ region, nrow = 2)

#We can see that the regions' the most affected by the conflicts are : Middle east and north Africa, Sub-saharan Africa, South Asia, then less America & the Caribbean and Eastern Europe

ggplot(data = conflicts_filtered, aes(x = year, y = pop_affected)) +
  geom_line() +
  labs(title = "Trend of Population Affected by Conflicts Over Time", x = "Year", y = "pop_affected")+
  facet_wrap(~ region, nrow = 2)  
##We can see that the regions' the most affected by the conflicts are : Middle east and north Africa, Sub-saharan Africa, South Asia, America & the Caribbean, Eastern Europe ans sometimes Caucasus and Central Asia

#Nicer --------> 

# Trend of Deaths by Conflicts Over Time
ggplot(data = conflicts_filtered, aes(x = year, y = sum_deaths, group = region, color = region)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.3, size = 0.5) +  # Using loess smoothing method
  labs(title = "Trend of Deaths by Conflicts Over Time", x = "Year", y = "Sum Deaths") +
  facet_wrap(~ region, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")


# Trend of Population Affected by Conflicts Over Time
ggplot(data = conflicts_filtered, aes(x = year, y = pop_affected, group = region, color = region)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.3, size = 0.5) +  # Using loess smoothing method
  labs(title = "Trend of Population Affected by Conflicts Over Time", x = "Year", y = "pop_affected") +
  facet_wrap(~ region, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")



#___________________________________________________________________________________________
#3. correlation Analysis per country -> irrelevant, I ll try to do it by region

# Extract relevant columns (goals and disaster variables) from Q3.1
disaster_data <- Q3.1[, c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "total_deaths", "no_injured", "no_affected", "no_homeless", "total_affected", "total_damages")]

# Compute correlation matrix
correlation_disaster <- cor(disaster_data)

# Visualize correlation matrix (optional)
corrplot::corrplot(correlation_disaster, method = "color")




# Extract relevant columns (goals and COVID-19 variables) from Q3.2
covid_data <- Q3.2[, c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "cases_per_million", "deaths_per_million", "stringency")]

# Compute correlation matrix
correlation_covid <- cor(covid_data)

# Visualize correlation matrix (optional)
corrplot::corrplot(correlation_covid, method = "color")




# Extract relevant columns (goals and conflict variables) from Q3.3
conflict_data <- Q3.3[, c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "pop_affected", "area_affected", "max_intensity")]

# Compute correlation matrix
correlation_conflict <- cor(conflict_data)

# Visualize correlation matrix (optional)
corrplot::corrplot(correlation_conflict, method = "color")





####### 3. correlation Analysis per Region#####_________________________________

# Get the names of the regions from the disaster dataset
regions <- unique(Q3.1$region)

# Group data by region and calculate mean for each variable
disaster_data_by_region <- Q3.1 %>%
  group_by(region) %>%
  summarize(across(starts_with("goal"), mean),  # Compute mean for goal variables
            across(total_deaths:total_damages, sum))  # Sum for disaster-related variables

# Compute correlation matrix for each region
correlation_disaster_by_region <- cor(disaster_data_by_region[, -1])

# Visualize correlation matrix for each region (optional)
corrplot::corrplot(correlation_disaster_by_region, method = "color")






grouped_data_by_region <- Q3.1 %>%
  group_by(region)

# Specify the list of disaster-related variables you want to correlate with SDG goals
disaster_variables <- c("total_affected", "total_deaths", "no_injured", "total_damages", "no_homeless")  # Update with your desired variables

# Compute correlations between SDG goals and multiple disaster-related variables for each region
correlations_by_region <- grouped_data_by_region %>%
  summarize(across(starts_with("goal"), ~cor(.x, across(all_of(disaster_variables)))), .groups = "drop")

# View correlations by region
print(correlations_by_region)





# Subset data for South and East Asia from Q3.1 dataset
south_east_asia_data <- Q3.1[Q3.1$region %in% c("South Asia", "East Asia"), ]


# Select relevant columns for correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "total_affected", "no_homeless")

# Compute correlation matrix for South and East Asia data
correlation_matrix_disaster_Asia <- cor(south_east_asia_data[, relevant_columns], use = "complete.obs")

# View the correlation matrix
print(correlation_matrix_disaster_Asia)

# Melt the correlation matrix for ggplot2
cor_melted <- as.data.frame(as.table(correlation_matrix_disaster_Asia))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Create the heatmap
ggplot(data = cor_melted, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Correlation between the climate disasters and the SDG goals in South and East Asia')







# Subset data for South and East Asia from Q3.1 dataset
south_east_asia_data <- Q3.1[Q3.1$region %in% c("South Asia", "East Asia"), ]


# Select relevant columns for correlation analysis
relevant_columns <- Q3.1[Q3.1$region %in% c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "total_affected", "no_homeless")]

# Compute correlation matrix for South and East Asia data
correlation_matrix_disaster_Asia <- cor(south_east_asia_data, relevant_columns, use = "complete.obs")

# View the correlation matrix
print(correlation_matrix_disaster_Asia)

# Melt the correlation matrix for ggplot2
cor_melted <- as.data.frame(as.table(correlation_matrix_disaster_Asia))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Create the heatmap
ggplot(data = cor_melted, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Correlation between the climate disasters and the SDG goals in South and East Asia')




##3. Correlation Analysis COVID###

# Filter COVID-19 data for the relevant time period
covid_filtered <- Q3.2[Q3.2$year >= as.Date("2019-01-01"), ]

# Select relevant columns for correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "stringency", "cases_per_million", "deaths_per_million")
# Subset data with relevant columns for correlation analysis
relevant_data <- covid_filtered[, relevant_columns]

# Compute correlation matrix for Covid Data
correlation_matrix_Covid <- cor(relevant_data, use = "complete.obs")

print(correlation_matrix_Covid)

# Melt the correlation matrix for ggplot2
cor_melted <- as.data.frame(as.table(correlation_matrix_Covid))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Create the heatmap
ggplot(data = cor_melted, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Correlation between COVID and the SDG goals')


##3. Correlation Analysis Conflicts###

# Filter data for specific regions
selected_regions <- c("Middle East & North Africa", "Sub-Saharan Africa", "South Asia", "Latin America & the Caribbean", "Eastern Europe")
conflicts_selected <- Q3.3[Q3.3$region %in% selected_regions, ]

# Select relevant columns for the correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "sum_deaths")

# Compute correlation matrix for the selected regions
correlation_matrix_Conflicts_Deaths <- cor(conflicts_selected[, relevant_columns], use = "complete.obs")

# View the correlation matrix
print(correlation_matrix_Conflicts_Deaths)

# Melt the correlation matrix for ggplot2
cor_melted <- as.data.frame(as.table(correlation_matrix_Conflicts_Deaths))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Create the heatmap
ggplot(data = cor_melted, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Correlation between Conflicts deaths and the SDG goals')




# Filter data for specific regions
selected_regions <- c("Latin America & the Caribbean")
conflicts_selected <- Q3.3[Q3.3$region %in% selected_regions, ]

# Select relevant columns for the correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "pop_affected")

# Compute correlation matrix for the selected regions
correlation_matrix_Conflicts_Deaths <- cor(conflicts_selected[, relevant_columns], use = "complete.obs")

# View the correlation matrix
print(correlation_matrix_Conflicts_Deaths)

# Melt the correlation matrix for ggplot2
cor_melted <- as.data.frame(as.table(correlation_matrix_Conflicts_Deaths))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Create the heatmap
ggplot(data = cor_melted, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Correlation between Conflicts deaths and the SDG goals')






# Filter data for specific regions (pop_affected)
selected_regions <- c("Middle East & North Africa", "Sub-Saharan Africa", "South Asia", "Latin America & the Caribbean", "Eastern Europe","Caucasus and Central Asia")
conflicts_selected <- Q3.3[Q3.3$region %in% selected_regions, ]

# Select relevant columns for the correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "pop_affected")

# Compute correlation matrix for the selected regions
correlation_matrix_Conflicts_Pop_Affected <- cor(conflicts_selected[, relevant_columns], use = "complete.obs")

# View the correlation matrix
print(correlation_matrix_Conflicts_Pop_Affected)

# Melt the correlation matrix for ggplot2
cor_melted <- as.data.frame(as.table(correlation_matrix_Conflicts_Pop_Affected))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Create the heatmap
ggplot(data = cor_melted, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Correlation between Conflicts Affected Population and the SDG goals')



###4. Regressions

#Disasters total affected, but which goal? 
# Perform linear regression
Lin_Reg_Disaster <- lm(goal1 ~ total_affected, data = Q3.1)

# Summary of the regression model
summary(Lin_Reg_Disaster)










