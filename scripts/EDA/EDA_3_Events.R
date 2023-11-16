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

# Explore descriptive statistics
summary(Q3.1)  # Display summary statistics for all variables


# Time-series analysis of SDG scores by region
ggplot(data = Q3.1, aes(x = year, y = overallscore)) +
  geom_line() +
  labs(title = "Trend of SDG Overall scores Over Time", x = "Year", y = "Overall SDG Score")+
  facet_wrap(~ region, nrow = 2)  # Modify nrow as per your preference for rows in facet grid


# Time-series analysis of COVID-19 cases per million by region
# Filter COVID-19 data for dates after 2020
covid_filtered <- Q3.2[Q3.2$year >= as.Date("2020-01-01"), ]

ggplot(data = covid_filtered, aes(x = year, y = cases_per_million)) +
  geom_line() +
  labs(title = "Trend of COVID-19 Cases per Million Over Time", x = "Year", y = "Cases per Million")+
  facet_wrap(~ region, nrow = 2)  # Modify nrow as per your preference for rows in facet grid


# Time-series analysis of climatic disasters (e.g., total affected)
ggplot(data = Q3.1, aes(x = year, y = total_affected)) +
  geom_line() +
  labs(title = "Trend of Total Affected from Climatic Disasters Over Time", x = "Year", y = "Total Affected")+
  facet_wrap(~ region, nrow = 2)  # Modify nrow as per your preference for rows in facet grid


# Time-series analysis of Conflicts
# Filter conflict data for the years between 2000 and 2016
conflicts_filtered <- Q3.3[Q3.3$year >= as.Date("2000-01-01") & Q3.3$year <= as.Date("2016-12-31"), ]

ggplot(data = conflicts_filtered, aes(x = year, y = pop_affected)) +
  geom_line() +
  labs(title = "Trend of Population affcted by Conflicts Over Time", x = "Year", y = "pop_affected")+
  facet_wrap(~ region, nrow = 2)  # Modify nrow as per your preference for rows in facet grid



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





####### 3. correlation Analysis per Region#####

# Get the names of the regions from the disaster dataset
regions <- unique(Q3.1$region)

# Visualize correlation matrix for each region with region names
corrplot::corrplot(
  correlation_disaster_by_region,
  method = "color",
  title = "Correlation Matrix for Disaster Dataset by Region",
  col.names = regions,
  row.names = regions
)

# Group data by region and calculate mean for each variable
disaster_data_by_region <- Q3.1 %>%
  group_by(region) %>%
  summarize(across(starts_with("goal"), mean),  # Compute mean for goal variables
            across(total_deaths:total_damages, sum))  # Sum for disaster-related variables

# Compute correlation matrix for each region
correlation_disaster_by_region <- cor(disaster_data_by_region[, -1])

# Visualize correlation matrix for each region (optional)
corrplot::corrplot(correlation_disaster_by_region, method = "color")



# Group data by region
grouped_data_by_region <- Q3.1 %>%
  group_by(region)

# Compute correlations between SDG goals and disaster-related variables for each region
correlations_by_region <- grouped_data_by_region %>%
  summarize(across(starts_with("goal"), ~cor(.x, total_affected)),  # Correlations with total_affected variable
            across(starts_with("goal"), ~cor(.x, total_deaths)),    # Correlations with total_deaths variable
            .groups = "drop")





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
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "total_deaths", "no_injured", "no_affected", "no_homeless", "total_affected", "total_damages")

# Compute correlation matrix for South and East Asia data
correlation_matrix <- cor(south_east_asia_data[, relevant_columns], use = "complete.obs")

# View the correlation matrix
print(correlation_matrix)

# Plotting the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", order = "original", 
         tl.col = "black", tl.srt = 45)




























#______________________________________________________________________________________-
##Disasters
head(Q3.1)
str(Q3.1)

# Replace the missing values by zero
Q3.1[is.na(Q3.1)] <- 0
print(Q3.1)


#Correlations between variables
Q3.1_Correlation <- Q3.1 %>% 
  select(overallscore:total_damages)

#Calculate correlation matrix
Q3.1_corr_matrix <- cor(Q3.1_Correlation, use = "everything")
print(Q3.1_corr_matrix)

## Heatmap
# Melt the correlation matrix
#cor_melted <- melt(Q3.1_corr_matrix)

# Create p-value matrix
cor_matrix <- matrix(nrow = ncol(Q3.1_corr_matrix), ncol = ncol(Q3.1_corr_matrix))
p_matrix <- matrix(nrow = ncol(Q3.1_corr_matrix), ncol = ncol(Q3.1_corr_matrix))
rownames(cor_matrix) <- colnames(Q3.1_corr_matrix)
rownames(p_matrix) <- colnames(Q3.1_corr_matrix)
colnames(cor_matrix) <- colnames(Q3.1_corr_matrix)
colnames(p_matrix) <- colnames(Q3.1_corr_matrix)


# Calculate p-values
for (i in 1:ncol(Q3.1_corr_matrix)) {
  for (j in 1:ncol(Q3.1_corr_matrix)) {
    # Exclude missing values before performing the correlation test
    non_missing_rows <- complete.cases(Q3.1_corr_matrix[, c(i, j)])
    
    if (sum(non_missing_rows) > 1) {  # Check if there are enough non-missing observations
      test_result <- cor.test(Q3.1_corr_matrix[non_missing_rows, i], 
                              Q3.1_corr_matrix[non_missing_rows, j])
      cor_matrix[i, j] <- test_result$estimate
      p_matrix[i, j] <- test_result$p.value
    } else {
      p_matrix[i, j] <- NA  # Set to NA if not enough non-missing observations
    }
  }
}

# Melt for ggplot2
melted_cor_matrix <-
  melt(cor_matrix)
melted_p_matrix <-
  melt(matrix(as.vector(p_matrix), nrow = ncol(Q3.1_corr_matrix)))

# Combine the datasets
plot_data <- cbind(melted_cor_matrix, p_value = melted_p_matrix$value)

# Create the ggplot2 plot
ggplot(data = plot_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  
  # Add text labels for correlation values and color based on significance
  geom_text(aes(label = sprintf("%.2f", value), color = p_value < 0.05),
            vjust = 1, size = 2.5) +
  
  # Customize the color scale
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  
  # Customize the color legend for significance
  scale_color_manual(values = c("black", "turquoise")) + # black when significant, yellow if not
  
  # Adjust the appearance of the plot
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 7, hjust = 1),
        axis.text.y = element_text(size= 7, hjust = 1),
        legend.position = "none") +
  
  # Little adjustments and labels
  coord_fixed() +
  labs(x = '', y = '', title = 'Correlation Matrix Heatmap Disaters')





#2ème??

#Correlations between variables
Q3.1_Correlation <- Q3.1 %>% 
  select(overallscore:total_damages)

# Calculate the correlation matrix
matrice_correlation <- cor(Q3.1_Correlation, use = "complete.obs")

# Print  the correlation matrix
print(matrice_correlation)


# Melt the correlation matrix for ggplot2
cor_melted <- as.data.frame(as.table(matrice_correlation))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Create the heatmap
ggplot(data = cor_melted, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Corrélation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8)) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Matrice de Corrélation et Heatmap')




#__________________________________________________________________________________


##COVID
Q3.2 <- read.csv(here("scripts", "data", "data_question3_2.csv"))
str(Q3.2)

#Correlations between variables
Q3.2_Correlation <- Q3.2 %>% 
  select(overallscore:stringency)

#Calculate correlation matrix
Q3.2_corr_matrix <- cor(Q3.2_Correlation, use = "everything")
print(Q3.2_corr_matrix)

## Heatmap

# Create p-value matrix
cor_matrix <- matrix(nrow = ncol(Q3.2_corr_matrix), ncol = ncol(Q3.2_corr_matrix))
p_matrix <- matrix(nrow = ncol(Q3.2_corr_matrix), ncol = ncol(Q3.2_corr_matrix))
rownames(cor_matrix) <- colnames(Q3.2_corr_matrix)
rownames(p_matrix) <- colnames(Q3.2_corr_matrix)
colnames(cor_matrix) <- colnames(Q3.2_corr_matrix)
colnames(p_matrix) <- colnames(Q3.2_corr_matrix)


# Calculate p-values
for (i in 1:ncol(Q3.2_corr_matrix)) {
  for (j in 1:ncol(Q3.2_corr_matrix)) {
    # Exclude missing values before performing the correlation test
    non_missing_rows <- complete.cases(Q3.2_corr_matrix[, c(i, j)])
    
    if (sum(non_missing_rows) > 1) {  # Check if there are enough non-missing observations
      test_result <- cor.test(Q3.2_corr_matrix[non_missing_rows, i], 
                              Q3.2_corr_matrix[non_missing_rows, j])
      cor_matrix[i, j] <- test_result$estimate
      p_matrix[i, j] <- test_result$p.value
    } else {
      p_matrix[i, j] <- NA  # Set to NA if not enough non-missing observations
    }
  }
}

# Melt for ggplot2
melted_cor_matrix <-
  melt(cor_matrix)
melted_p_matrix <-
  melt(matrix(as.vector(p_matrix), nrow = ncol(Q3.2_corr_matrix)))

# Combine the datasets
plot_data <- cbind(melted_cor_matrix, p_value = melted_p_matrix$value)

# Create the ggplot2 plot
ggplot(data = plot_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  
  # Add text labels for correlation values and color based on significance
  geom_text(aes(label = sprintf("%.2f", value), color = p_value < 0.05),
            vjust = 1, size = 2.5) +
  
  # Customize the color scale
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  
  # Customize the color legend for significance
  scale_color_manual(values = c("black", "turquoise")) + # black when significant, yellow if not
  
  # Adjust the appearance of the plot
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 7, hjust = 1),
        axis.text.y = element_text(size=7, hjust = 1),
        legend.position = "none") +
  
  # Little adjustments and labels
  coord_fixed() +
  labs(x = '', y = '', title = 'Correlation Matrix Heatmap Covid')




#____________________________________________________________________________________
##Conflicts
Q3.3 <- read.csv(here("scripts", "data", "data_question3_3.csv"))
str(Q3.3)

#Correlations between variables
Q3.3_Correlation <- Q3.3 %>% 
  select(overallscore:maxintensity)

#Calculate correlation matrix
Q3.3_corr_matrix <- cor(Q3.3_Correlation, use = "everything")
print(Q3.2_corr_matrix)

## Heatmap

# Create p-value matrix
cor_matrix <- matrix(nrow = ncol(Q3.3_corr_matrix), ncol = ncol(Q3.3_corr_matrix))
p_matrix <- matrix(nrow = ncol(Q3.3_corr_matrix), ncol = ncol(Q3.3_corr_matrix))
rownames(cor_matrix) <- colnames(Q3.3_corr_matrix)
rownames(p_matrix) <- colnames(Q3.3_corr_matrix)
colnames(cor_matrix) <- colnames(Q3.3_corr_matrix)
colnames(p_matrix) <- colnames(Q3.3_corr_matrix)


# Calculate p-values
for (i in 1:ncol(Q3.3_corr_matrix)) {
  for (j in 1:ncol(Q3.3_corr_matrix)) {
    # Exclude missing values before performing the correlation test
    non_missing_rows <- complete.cases(Q3.3_corr_matrix[, c(i, j)])
    
    if (sum(non_missing_rows) > 1) {  # Check if there are enough non-missing observations
      test_result <- cor.test(Q3.3_corr_matrix[non_missing_rows, i], 
                              Q3.3_corr_matrix[non_missing_rows, j])
      cor_matrix[i, j] <- test_result$estimate
      p_matrix[i, j] <- test_result$p.value
    } else {
      p_matrix[i, j] <- NA  # Set to NA if not enough non-missing observations
    }
  }
}

# Melt for ggplot2
melted_cor_matrix <-
  melt(cor_matrix)
melted_p_matrix <-
  melt(matrix(as.vector(p_matrix), nrow = ncol(Q3.3_corr_matrix)))

# Combine the datasets
plot_data <- cbind(melted_cor_matrix, p_value = melted_p_matrix$value)

# Create the ggplot2 plot
ggplot(data = plot_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  
  # Add text labels for correlation values and color based on significance
  geom_text(aes(label = sprintf("%.2f", value), color = p_value < 0.05),
            vjust = 1, size = 2.5) +
  
  # Customize the color scale
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  
  # Customize the color legend for significance
  scale_color_manual(values = c("black", "turquoise")) + # black when significant, yellow if not
  
  # Adjust the appearance of the plot
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 7, hjust = 1),
        axis.text.y = element_text(size=7, hjust = 1),
        legend.position = "none") +
  
  # Little adjustments and labels
  coord_fixed() +
  labs(x = '', y = '', title = 'Correlation Matrix Heatma Conflicts')

