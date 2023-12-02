### Question 3 ####

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






##Correlations disasters###

# Subset data for South and East Asia from Q3.1 dataset
south_east_asia_data <- Q3.1[Q3.1$region %in% c("South Asia", "East Asia"), ]

# Select relevant columns for correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "total_affected", "no_homeless")

# Subset the data
subset_data <- south_east_asia_data[, relevant_columns]

# Compute correlation matrix for total_affected, no_homeless with the rest of the variables
correlation_matrix_subset <- cor(subset_data[, c("total_affected", "no_homeless")], subset_data)

# Melt the correlation matrix for ggplot2
cor_melted <- reshape2::melt(correlation_matrix_subset)
names(cor_melted) <- c("Variable2", "Variable1", "Correlation")

# Create the heatmap
library(ggplot2)

ggplot(data = cor_melted, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
         axis.text.y = element_text(vjust = 1, size = 8, hjust = 2),
         plot.title = element_text(margin = margin(b = 20), hjust = 0.5, 
                                   vjust = 2.5, lineheight = 1.5)
  ) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Correlation between the climate disasters and the SDG goals in South and East Asia')






##3. Correlation Analysis COVID###

# Filter COVID-19 data for the relevant time period
covid_filtered <- Q3.2[Q3.2$year >= as.Date("2018-12-31"), ]

# Select relevant columns for correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "stringency", "cases_per_million", "deaths_per_million")

# Subset data with relevant columns for correlation analysis
subset_data <- covid_filtered[, relevant_columns]

# Compute correlation matrix for "stringency", "cases_per_million", "deaths_per_million" with the rest of the variables
correlation_matrix_Covid <- cor(subset_data, subset_data[, c("stringency", "cases_per_million", "deaths_per_million")])

# Melt the correlation matrix for ggplot2
cor_melted <- as.data.frame(as.table(correlation_matrix_Covid))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Create the heatmap
library(ggplot2)

ggplot(data = cor_melted, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
         axis.text.y = element_text(vjust = 1, size = 8, hjust = 1),
         plot.title = element_text(margin = margin(b = 20), hjust = 0.5, 
                                   vjust = 5, lineheight = 1.5)
  ) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Correlation between COVID and the SDG goals')

print(correlation_matrix_Covid)




##3. Correlation Analysis Conflicts###


Q3.3 <- read.csv(here("scripts", "data", "data_question3_3.csv"))


# Filter data for specific regions (sum_deaths)
conflicts_filtered <- Q3.3[Q3.3$region %in% c("Middle East & North Africa", "Sub-Saharan Africa", "South Asia"), ]

# Select relevant columns for the correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "sum_deaths")

# Subset data with relevant columns for correlation analysis
subset_data <- conflicts_filtered[, relevant_columns]

# Compute correlation matrix for "sum_deaths" with the rest of the variables
correlation_matrix_Conflicts_Deaths <- cor(subset_data, subset_data[, c("sum_deaths")])

# Melt the correlation matrix for ggplot2
cor_melted <- as.data.frame(as.table(correlation_matrix_Conflicts_Deaths))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Create the heatmap
library(ggplot2)

ggplot(data = cor_melted, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
         axis.text.y = element_text(vjust = 1, size = 8, hjust = 1),
         plot.title = element_text(margin = margin(b = 20), hjust = 0.5, 
                                   vjust = 8, lineheight = 2)
  ) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Correlation between Conflicts deaths and the SDG goals')


print(correlation_matrix_Conflicts_Deaths)






# Filter data for specific regions (pop_affected) and (sum_deaths), as the regions most touched by the sum_deaths are the same regions that have also the must affected population.

conflicts_filtered <- Q3.3[Q3.3$region %in% c("Middle East & North Africa", "Sub-Saharan Africa", "South Asia", "Latin America & the Caribbean", "Eastern Europe", "Caucasus and Central Asia"), ]

# Select relevant columns for the correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "pop_affected", "sum_deaths")

# Subset data with relevant columns for correlation analysis
subset_data <- conflicts_filtered[, relevant_columns]

# Compute correlation matrix for "pop_affected" with the rest of the variables
correlation_matrix_Conflicts_Pop_Aff <- cor(subset_data, subset_data[, c("pop_affected", "sum_deaths")])

# Melt the correlation matrix for ggplot2
cor_melted <- as.data.frame(as.table(correlation_matrix_Conflicts_Pop_Aff))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Create the heatmap
ggplot(data = cor_melted, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
         axis.text.y = element_text(vjust = 1, size = 8, hjust = 1),
         plot.title = element_text(margin = margin(b = 20), hjust = 0.5, 
                                   vjust = 8, lineheight = 2)
  ) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Correlation between Conflicts Affected Population and the SDG goals')







### We asked ourselves if the fact that we do not see any correlations is because the consequences of this disasters arrive later on, so we could remake the same correlations with 1 year of gap.

##Correlations disasters###

# Subset data for South and East Asia from Q3.1 dataset
south_east_asia_data <- Q3.1[Q3.1$region %in% c("South Asia", "East Asia"), ]

# Select relevant columns for correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "total_affected", "no_homeless")

# Subset the data
subset_data <- south_east_asia_data[, relevant_columns]

# Create lagged variables with a one-year gap for disaster-related columns
lagged_subset_data <- subset_data %>%
  mutate(
    lagged_total_affected = lag(total_affected, default = NA),
    lagged_no_homeless = lag(no_homeless, default = NA)
  )


# Compute correlation matrix for lagged disaster-related variables with the rest of the variables
correlation_matrix_lagged <- cor(lagged_subset_data[, c("lagged_total_affected", "lagged_no_homeless")], subset_data)

# Melt the correlation matrix for ggplot2
cor_melted_lagged <- reshape2::melt(correlation_matrix_lagged)
names(cor_melted_lagged) <- c("Variable2", "Variable1", "Correlation")

# Create the heatmap
library(ggplot2)

ggplot(data = cor_melted_lagged, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
         axis.text.y = element_text(vjust = 1, size = 8, hjust = 2),
         plot.title = element_text(margin = margin(b = 20), hjust = 0.5, 
                                   vjust = 6, lineheight = 1.5)
  ) +
  coord_fixed() +
  labs(x = '', y = '',
       title = 'Correlation between the climate disasters and the SDG goals in South and East Asia with 1 year gap')





#Now I tried to do the same but with an interactive heat map

#***So here's an interactive map of the correlation between the climate disasters and the SDG goals in South and East Asia with 1 year gap

# Load necessary libraries
library(shiny)
library(plotly)

# Subset data for South and East Asia from Q3.1 dataset
south_east_asia_data <- Q3.1[Q3.1$region %in% c("South Asia", "East Asia"), ]

# Select relevant columns for correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "total_affected", "no_homeless")

# Subset the data
subset_data <- south_east_asia_data[, relevant_columns]

# Create lagged variables with a one-year gap for disaster-related columns
lagged_subset_data <- subset_data %>%
  mutate(
    lagged_total_affected = lag(total_affected, default = NA),
    lagged_no_homeless = lag(no_homeless, default = NA)
  )

# Compute correlation matrix for lagged disaster-related variables with the rest of the variables
correlation_matrix_lagged <- cor(lagged_subset_data[, c("lagged_total_affected", "lagged_no_homeless")], subset_data)

# Melt the correlation matrix for ggplot2
cor_melted_lagged <- reshape2::melt(correlation_matrix_lagged)
names(cor_melted_lagged) <- c("Variable2", "Variable1", "Correlation")

# UI code for creating the interactive heatmap
ui <- fluidPage(
  titlePanel("Interactive Correlation Heatmap between the climate disasters and the SDG goals in South and East Asia with 1 year gap"),
  plotlyOutput("heatmap"),
  sliderInput("year", "Select Year", min = 2000, max = 2021, value = 2012, step = 1)
)

# Server code for the interactive heatmap
server <- function(input, output) {
  # Create a reactive expression for selecting data based on the chosen year
  selected_data <- reactive({
    # Filter your dataset based on the selected year
    filtered_data <- south_east_asia_data[south_east_asia_data$year == input$year, ]
    # Select relevant columns for correlation analysis
    subset_data <- filtered_data[, relevant_columns]
    # Create lagged variables with a one-year gap for disaster-related columns
    lagged_subset_data <- subset_data %>%
      mutate(
        lagged_total_affected = lag(total_affected, default = NA),
        lagged_no_homeless = lag(no_homeless, default = NA)
      )
    
    # Compute correlation matrix for lagged disaster-related variables with the rest of the variables
    correlation_matrix_lagged <- cor(lagged_subset_data[, c("lagged_total_affected", "lagged_no_homeless")], subset_data)
    
    # Melt the correlation matrix for plotly
    cor_melted_lagged <- reshape2::melt(correlation_matrix_lagged)
    names(cor_melted_lagged) <- c("Variable2", "Variable1", "Correlation")
    
    return(cor_melted_lagged)
  })
  
  
  # Render the plotly heatmap based on the reactive selected_data
  output$heatmap <- renderPlotly({
    # plotly creates an interactive heatmap
    #'selected_data()' reflects the chosen year's correlations
    p <- plot_ly(data = selected_data(), x = ~Variable1, y = ~Variable2, z = ~Correlation, 
                 type = "heatmap", colorscale = list(c(-1, "blue"), c(0, "white"), c(1, "red")),
                 zmin = -1, zmax = 1)
    
    # Customize the layout, axis labels, and other aspects of the plot as needed
    p <- p %>% layout(
      title = "",
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      coloraxis = list(
        colorbar = list(
          title = "Correlation",  # Title for the color bar
          tickvals = c(-1, 0, 1),  # Define tick values for the color bar
          ticktext = c("-1", "0", "1"),  # Define corresponding text for tick values
          len = 5,  # Length of the color bar
          thickness = 20,  # Thickness of the color bar
          x = 0,  # Position of the color bar on x-axis (0 = left side)
          xanchor = "left",  # Anchor the color bar to the left
          ticks = "outside"  # Show color bar ticks outside the color bar
        )
      )
    )
    # Return the plotly object
    return(p)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

## In the provided code, when you select a particular year (let's say 2023 or "year 23" as an example), the lagged variables created using the lag() function represent the values from the previous year, which would be 2022 ("year 22" in this case).

##So, if you are looking at the data for the year 2023, the lagged variables (lagged_total_affected and lagged_no_homeless) will contain the values from the year 2022. The code uses the lag() function to shift the values by one position, effectively taking the values from the immediate preceding year to create the lagged variables.

##Therefore, if you select a specific year (e.g., 2023) in the app, the analysis will show correlations between the SDG indicators for the selected year (e.g., 2023) and the disaster-related variables (total_affected and no_homeless) from the previous year (e.g., 2022).


##----> still nothing :(




### # 3. Correlation Analysis COVID
# Load necessary libraries
library(shiny)
library(plotly)

Q3.2 <- read.csv(here("scripts", "data", "data_question3_2.csv"))

#covid_filtered <- Q3.2[Q3.2$year >= as.Date("2018-12-31"), ]
covid_filtered <- Q3.2
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "stringency", "cases_per_million", "deaths_per_million")
subset_data <- covid_filtered[, relevant_columns]

# Compute correlation matrix for "stringency", "cases_per_million", "deaths_per_million" with the rest of the variables
correlation_matrix_Covid <- cor(subset_data, subset_data[, c("stringency", "cases_per_million", "deaths_per_million")])

# Melt the correlation matrix for plotly
cor_melted <- as.data.frame(as.table(correlation_matrix_Covid))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

# Shiny UI code
ui <- fluidPage(
  titlePanel("Interactive Correlation Heatmap between COVID and the SDG goal with one year gap"),
  plotlyOutput("heatmap"),
  sliderInput("year", "Select Year", min = 2019, max = 2023, value = 2020, step = 1)
)

# Shiny server code
server <- function(input, output) {
  # Reactive expression for selected COVID data based on the chosen year
  selected_covid_data <- reactive({
    filtered_data <- covid_filtered[covid_filtered$year == input$year, ]
    subset_data <- filtered_data[, relevant_columns]
    return(subset_data)
  })
  
  # Render the plotly heatmap based on the reactive selected_covid_data
  output$heatmap <- renderPlotly({
    correlation_matrix_Covid <- cor(selected_covid_data(), selected_covid_data()[, c("stringency", "cases_per_million", "deaths_per_million")])
    cor_melted <- as.data.frame(as.table(correlation_matrix_Covid))
    names(cor_melted) <- c("Variable1", "Variable2", "Correlation")
    
    p <- plot_ly(data = cor_melted, x = ~Variable1, y = ~Variable2, z = ~Correlation,
                 type = "heatmap", colorscale = list(c(0, "blue"), c(0.5, "white"), c(1, "red")),
                 zmin = -1, zmax = 1)
    
    p <- p %>% layout(
      title = "",
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      coloraxis = list(
        colorbar = list(
          title = "Correlation",  # Title for the color bar
          tickvals = c(-1, 0, 1),  # Define tick values for the color bar
          ticktext = c("-1", "0", "1"),  # Define corresponding text for tick values
          len = 5,  # Length of the color bar
          thickness = 20,  # Thickness of the color bar
          x = 0,  # Position of the color bar on x-axis (0 = left side)
          xanchor = "left",  # Anchor the color bar to the left
          ticks = "outside"  # Show color bar ticks outside the color bar
        )
      )
    )
    # Return the plotly object
    return(p)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)




###Conflict correlation for disaster with interactive heatmap with one year gap

# Load necessary libraries
library(shiny)
library(plotly)

# Filter data for specific regions (pop_affected) and (sum_deaths), as the regions most touched by the sum_deaths are the same regions that have also the must affected population.
conflicts_filtered <- Q3.3[Q3.3$region %in% c("Middle East & North Africa", "Sub-Saharan Africa", "South Asia", "Latin America & the Caribbean", "Eastern Europe", "Caucasus and Central Asia"), ]

# Select relevant columns for the correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "pop_affected", "sum_deaths")

# Subset data with relevant columns for correlation analysis
subset_data <- conflicts_filtered[, relevant_columns]

# Compute correlation matrix for "pop_affected" with the rest of the variables
correlation_matrix_Conflicts_Pop_Aff <- cor(subset_data, subset_data[, c("pop_affected", "sum_deaths")])

# Melt the correlation matrix for ggplot2
cor_melted <- as.data.frame(as.table(correlation_matrix_Conflicts_Pop_Aff))
names(cor_melted) <- c("Variable1", "Variable2", "Correlation")

#selected regions:Middle East & North Africa", "Sub-Saharan Africa", "South Asia", "Latin America & the Caribbean", "Eastern Europe", "Caucasus and Central Asia"
# Shiny UI code
ui <- fluidPage(
  titlePanel("Interactive Correlation Heatmap between Conflicts in selected regions and the SDG goal with one year gap"),
  plotlyOutput("heatmap"),
  sliderInput("year", "Select Year", min = 2000, max = 2016, value = 2005, step = 1)
)

# Shiny server code
server <- function(input, output) {
  # Reactive expression for selected Conflict data based on the chosen year
  selected_conflicts_data <- reactive({
    filtered_data <- conflicts_filtered[conflicts_filtered$year == input$year, ]
    subset_data <- filtered_data[, relevant_columns]
    return(subset_data)
  })
  
  # Render the plotly heatmap based on the reactive selected_conflicts_data
  output$heatmap <- renderPlotly({
    correlation_matrix_Conflicts_Pop_Aff <- cor(selected_conflicts_data(), selected_conflicts_data()[, c("pop_affected", "sum_deaths")])
    cor_melted <- as.data.frame(as.table(correlation_matrix_Conflicts_Pop_Aff))
    names(cor_melted) <- c("Variable1", "Variable2", "Correlation")
    
    p <- plot_ly(data = cor_melted, x = ~Variable1, y = ~Variable2, z = ~Correlation,
                 type = "heatmap", colorscale = list(c(-1, "blue"), c(0, "white"), c(1, "red")),
                 zmin = -1, zmax = 1)
    
    p <- p %>% layout(
      title = "",
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      coloraxis = list(
        colorbar = list(
          title = "Correlation",  # Title for the color bar
          tickvals = c(-1, 0, 1),  # Define tick values for the color bar
          ticktext = c("-1", "0", "1"),  # Define corresponding text for tick values
          len = 5,  # Length of the color bar
          thickness = 20,  # Thickness of the color bar
          x = 0,  # Position of the color bar on x-axis (0 = left side)
          xanchor = "left",  # Anchor the color bar to the left
          ticks = "outside"  # Show color bar ticks outside the color bar
        )
      )
    )
    # Return the plotly object
    return(p)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)




###4. Regressions

#Disasters total affected, but which goal? 
# Perform linear regression
Lin_Reg_Disaster <- lm(goal1 ~ total_affected, data = Q3.1)

# Summary of the regression model
summary(Lin_Reg_Disaster)





Q3.1 <- read.csv(here("scripts", "data", "data_question3_1.csv"))
Q3.2 <- read.csv(here("scripts", "data", "data_question3_2.csv"))
Q3.3 <- read.csv(here("scripts", "data", "data_question3_3.csv"))

Q3.1[is.na(Q3.1)] <- 0
print(Q3.1)


##_____
# Subset data for South and East Asia from Q3.1 dataset
south_east_asia_data <- Q3.1[Q3.1$region %in% c("South Asia", "East Asia"), ]

# Select relevant columns for correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "total_affected", "no_homeless")

# Subset the data
subset_data <- south_east_asia_data[, relevant_columns]

# Define the specific goal columns you want to include in regression
goal_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16")


# Loop through each goal column and perform regressions
for (goal_col in relevant_columns) { 
  # Formula for regression against total_affected and no_homeless
  formula_affected <- as.formula(paste(goal_col, "~ total_affected"))
  formula_homeless <- as.formula(paste(goal_col, "~ no_homeless"))
  
  # Perform linear regression for total_affected
  lm_total_affected <- lm(formula_affected, data = subset_data)
  
  # Perform linear regression for no_homeless
  lm_no_homeless <- lm(formula_homeless, data = subset_data)
  
  # Print regression summaries
  cat("Regression Summary for", goal_col, "vs Total Affected:\n")
  print(summary(lm_total_affected))
  cat("\n")
  
  cat("Regression Summary for", goal_col, "vs No Homeless:\n")
  print(summary(lm_no_homeless))
  cat("\n")
}


# Sample goals
goals <- c("Goal 1", "Goal 2", "Goal 3", "Goal 4")

# UI part of the Shiny app
ui <- fluidPage(
  titlePanel("Goal Tracker"),
  sidebarLayout(
    sidebarPanel(
      h4("Goals"),
      checkboxGroupInput("goal_checkboxes", "Select Goals", choices = goals)
    ),
    mainPanel(
      h4("Selected Goals"),
      verbatimTextOutput("selected_goals")
    )
  )
)

# Server part of the Shiny app
server <- function(input, output) {
  output$selected_goals <- renderPrint({
    selected <- input$goal_checkboxes
    if (is.null(selected) || length(selected) == 0) {
      return("No goals selected.")
    } else {
      paste("Selected goals:", paste(selected, collapse = ", "))
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)




library(shiny)
library(dplyr)
library(ggplot2)
library(scales)


# Subset data for South and East Asia from Q3.1 dataset
south_east_asia_data <- Q3.1[Q3.1$region %in% c("South Asia", "East Asia"), ]

# Select relevant columns for correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "total_affected", "no_homeless")

# Subset the data
subset_data <- south_east_asia_data[, relevant_columns]

# Define the specific goal columns you want to include in regression
goal_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16")


# UI for Shiny app
ui <- fluidPage(
  titlePanel("SDG Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sdg", "Select SDG Goal:",
                  choices = goal_columns,
                  selected = goal_columns[1]),
      width = 3
    ),
    mainPanel(
      width = 9,
      plotOutput("regression_plot_affected"),
      plotOutput("regression_plot_homeless")
    )
  )
)

# Server logic
server <- function(input, output) {
  # Function to generate regression plots
  generate_regression_plot <- function(selected_goal) {
    # Formula for regression against total_affected and no_homeless
    formula_affected <- as.formula(paste(selected_goal, "~ total_affected"))
    formula_homeless <- as.formula(paste(selected_goal, "~ no_homeless"))
    
    # Perform linear regression for total_affected
    lm_total_affected <- lm(formula_affected, data = subset_data)
    
    # Plotting regression results
    plot_total_affected <- ggplot(subset_data, aes(x = total_affected, y = !!as.name(selected_goal))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Regression plot for", selected_goal, "vs Total Affected"),
           x = "Total Affected", y = selected_goal) +
      scale_x_continuous(labels = comma_format())  # Format x-axis labels for total_affected
    
    plot_no_homeless <- ggplot(subset_data, aes(x = no_homeless, y = !!as.name(selected_goal))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Regression plot for", selected_goal, "vs No Homeless"),
           x = "No Homeless", y = selected_goal) +
      scale_x_continuous(labels = comma_format())  # Format x-axis labels for no_homeless
    
    
    list(plot_total_affected, plot_no_homeless)
  }
  
  # Render regression plots based on selected SDG goal
  output$regression_plot_affected <- renderPlot({
    selected_goal <- input$sdg
    regression_plots <- generate_regression_plot(selected_goal)
    print(regression_plots[[1]])  # Display plot for total_affected vs selected_goal
  })
  
  output$regression_plot_homeless <- renderPlot({
    selected_goal <- input$sdg
    regression_plots <- generate_regression_plot(selected_goal)
    print(regression_plots[[2]])  # Display plot for no_homeless vs selected_goal
  })
}

# Run the application
shinyApp(ui, server)

#Model 1: Regression of goal1 vs Total Affected:
# The coefficient for the predictor variable "total_affected" is -3.41e-08 with a standard error of 4.19e-08. This indicates a very small negative relationship between the predictor and the outcome "goal1." However, the p-value (0.42) is larger than the typical significance level of 0.05, suggesting that the relationship between "total_affected" and "goal1" is not statistically significant.

#Model 2: Regression of goal1 vs No Homeless:
#The coefficient for the predictor variable "no_homeless" is -3.82e-06 with a standard error of 3.71e-06. This indicates a small negative relationship between the predictor and the outcome "goal1." However, similarly, the p-value (0.3) is larger than 0.05, suggesting that the relationship between "no_homeless" and "goal1" is not statistically significant.

#For "goal2," the variable "total_affected" shows a slight positive association that is almost statistically significant.
#The variable "no_homeless" does not appear to have a statistically significant impact on "goal2."

#For "goal3," "total_affected" does not have a statistically significant impact, and it seems to be an irrelevant predictor as per this model.
#On the other hand, "no_homeless" shows a statistically significant negative relationship with "goal3," although the effect size is small. This means that the presence of homeless people might have a small negative impact on achieving "goal3."
#However, considering the low R-squared values in both models, there might be other unaccounted factors that better explain the variation in "goal3."

#For "goal4" and "goal5," both "total_affected" and "no_homeless" variables do not show statistically significant relationships in their respective models. The explanatory power of these predictors is quite weak for both goals.

#For "goal6," both "total_affected" and "no_homeless" show statistically significant relationships, albeit with small effect sizes.
#For "goal7," neither "total_affected" nor "no_homeless" appear to have statistically significant relationships, and their contribution to explaining the variability in "goal7" is extremely weak based on the regression models.

#For "goal8" and "goal9," neither "total_affected" nor "no_homeless" seem to have statistically significant relationships based on the regression models. Their contributions to explaining the variability in these goals are extremely weak or negligible given the high p-values and low R-squared values in both cases.

#For "goal10," "Total Affected" shows a significant negative relationship, while "No Homeless" doesn't significantly explain the variation in "goal10."
#For "goal11," both "Total Affected" and "No Homeless" have significant negative relationships, indicating their impact on "goal11."
#for "goal12," both "Total Affected" and "No Homeless" are significantly positively associated. However, for "goal13," "No Homeless" shows a marginal association, while "Total Affected" doesn't significantly explain the variability.

#for "goal15," neither "Total Affected" nor "No Homeless" significantly explains the variability. However, for "goal16," both "Total Affected" and "No Homeless" are significantly negatively associated.



#___________________________
#Covid regressions

library(shiny)
library(ggplot2)
library(scales)

# Read the data
Q3.2 <- read.csv(here("scripts", "data", "data_question3_2.csv"))
covid_filtered <- Q3.2

# Select relevant columns for correlation analysis
relevant_columns <- c(
  "goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8",
  "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16",
  "stringency", "cases_per_million", "deaths_per_million"
)
subset_data <- covid_filtered[, relevant_columns]

# Define the specific goal columns you want to include in regression
goal_columns <- c(
  "goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8",
  "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16"
)

# UI for Shiny app
ui <- fluidPage(
  titlePanel("SDG - COVID Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sdg", "Select SDG Goal:",
                  choices = goal_columns,
                  selected = goal_columns[1]),
      width = 3
    ),
    mainPanel(
      width = 9,
      plotOutput("regression_plot_stringency"),
      plotOutput("regression_plot_cases"),
      plotOutput("regression_plot_deaths")
    )
  )
)

# Server logic
server <- function(input, output) {
  # Function to generate regression plots
  generate_regression_plot <- function(selected_goal) {
    # Formulas for regression against COVID-19 variables
    formula_stringency <- as.formula(paste(selected_goal, "~ stringency"))
    formula_cases <- as.formula(paste(selected_goal, "~ cases_per_million"))
    formula_deaths <- as.formula(paste(selected_goal, "~ deaths_per_million"))
    
    # Perform linear regression for stringency
    lm_stringency <- lm(formula_stringency, data = subset_data)
    lm_cases <- lm(formula_cases, data = subset_data)
    lm_deaths <- lm(formula_deaths, data = subset_data)
    
    # Plotting regression results
    plot_stringency <- ggplot(subset_data, aes(x = stringency, y = !!as.name(selected_goal))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Regression plot for", selected_goal, "vs Stringency"),
           x = "Stringency", y = selected_goal) +
      scale_x_continuous(labels = comma_format())  # Format x-axis labels for stringency
    
    plot_cases <- ggplot(subset_data, aes(x = cases_per_million, y = !!as.name(selected_goal))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Regression plot for", selected_goal, "vs Cases per Million"),
           x = "Cases per Million", y = selected_goal) +
      scale_x_continuous(labels = comma_format())  # Format x-axis labels for cases_per_million
    
    plot_deaths <- ggplot(subset_data, aes(x = deaths_per_million, y = !!as.name(selected_goal))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Regression plot for", selected_goal, "vs Deaths per Million"),
           x = "Deaths per Million", y = selected_goal) +
      scale_x_continuous(labels = comma_format())  # Format x-axis labels for deaths_per_million
    
    list(plot_stringency, plot_cases, plot_deaths)
  }
  
  # Render regression plots based on selected SDG goal
  output$regression_plot_stringency <- renderPlot({
    selected_goal <- input$sdg
    regression_plots <- generate_regression_plot(selected_goal)
    print(regression_plots[[1]])  # Display plot for stringency vs selected_goal
  })
  
  output$regression_plot_cases <- renderPlot({
    selected_goal <- input$sdg
    regression_plots <- generate_regression_plot(selected_goal)
    print(regression_plots[[2]])  # Display plot for cases_per_million vs selected_goal
  })
  
  output$regression_plot_deaths <- renderPlot({
    selected_goal <- input$sdg
    regression_plots <- generate_regression_plot(selected_goal)
    print(regression_plots[[3]])  # Display plot for deaths_per_million vs selected_goal
  })
}

# Run the application
shinyApp(ui, server)


#___________________
#Conflicts regression 


library(shiny)
library(ggplot2)
library(scales)

# Filter data for specific regions (pop_affected) and (sum_deaths)
conflicts_filtered <- Q3.3[Q3.3$region %in% c("Middle East & North Africa", "Sub-Saharan Africa", "South Asia", "Latin America & the Caribbean", "Eastern Europe", "Caucasus and Central Asia"), ]

# Select relevant columns for the correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "pop_affected", "sum_deaths")

# Subset data with relevant columns for correlation analysis
subset_data <- conflicts_filtered[, relevant_columns]

# Define the specific goal columns you want to include in regression
goal_columns <- c(
  "goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8",
  "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16"
)

# UI for Shiny app
ui <- fluidPage(
  titlePanel("SDG - Conflicts Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sdg", "Select SDG Goal:",
                  choices = goal_columns,
                  selected = goal_columns[1]),
      width = 3
    ),
    mainPanel(
      width = 9,
      plotOutput("regression_plot_pop_affected"),
      plotOutput("regression_plot_sum_deaths")
    )
  )
)

# Server logic
server <- function(input, output) {
  # Function to generate regression plots
  generate_regression_plot <- function(selected_goal) {
    # Formulas for regression against conflict variables
    formula_pop_affected <- as.formula(paste(selected_goal, "~ pop_affected"))
    formula_sum_deaths <- as.formula(paste(selected_goal, "~ sum_deaths"))
    
    # Perform linear regression for pop_affected and sum_deaths
    lm_pop_affected <- lm(formula_pop_affected, data = subset_data)
    lm_sum_deaths <- lm(formula_sum_deaths, data = subset_data)
    
    # Plotting regression results
    plot_pop_affected <- ggplot(subset_data, aes(x = pop_affected, y = !!as.name(selected_goal))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Regression plot for", selected_goal, "vs Population Affected"),
           x = "Population Affected", y = selected_goal) +
      scale_x_continuous(labels = comma_format())  # Format x-axis labels for pop_affected
    
    plot_sum_deaths <- ggplot(subset_data, aes(x = sum_deaths, y = !!as.name(selected_goal))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Regression plot for", selected_goal, "vs Sum of Deaths"),
           x = "Sum of Deaths", y = selected_goal) +
      scale_x_continuous(labels = comma_format())  # Format x-axis labels for sum_deaths
    
    list(plot_pop_affected, plot_sum_deaths)
  }
  
  # Render regression plots based on selected SDG goal
  output$regression_plot_pop_affected <- renderPlot({
    selected_goal <- input$sdg
    regression_plots <- generate_regression_plot(selected_goal)
    print(regression_plots[[1]])  # Display plot for pop_affected vs selected_goal
  })
  
  output$regression_plot_sum_deaths <- renderPlot({
    selected_goal <- input$sdg
    regression_plots <- generate_regression_plot(selected_goal)
    print(regression_plots[[2]])  # Display plot for sum_deaths vs selected_goal
  })
}

# Run the application
shinyApp(ui, server)




















#regression sur difference de scors que sur scors? (Delia) 
