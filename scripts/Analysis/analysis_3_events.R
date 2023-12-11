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
disaster_data <- Q3.1[Q3.1$region %in% c("South Asia", "East Asia", "North America"), ]

# Select relevant columns for correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "total_affected", "no_homeless")

# Subset the data
subset_data <- disaster_data[, relevant_columns]

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






#disaster
# List of goals to analyze
disaster_data <- Q3.1[Q3.1$region %in% c("South Asia", "East Asia", " North America"), ]
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16", "total_affected", "total_deaths")
subset_data <- disaster_data[, relevant_columns]
selected_goals <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16")  # Add more goals as needed

# Loop through each selected goal
for (goal in selected_goals) {
  # Formula for regression against total_affected and no_homeless
  formula_affected <- as.formula(paste(goal, "~ total_affected"))
  formula_deaths <- as.formula(paste(goal, "~ total_deaths"))
  
  # Perform linear regression for total_affected and no_homeless
  lm_total_affected <- lm(formula_affected, data = subset_data)
  lm_deaths <- lm(formula_deaths, data = subset_data)
  
  # Create summary output for the regression results
  summary_total_affected <- summary(lm_total_affected)
  summary_deaths <- summary(lm_deaths)
  
  # Print or save the regression summaries
  cat("\nRegression Summary for", goal, "vs Total Affected:\n")
  print(summary_total_affected)
  
  cat("\nRegression Summary for", goal, "vs No Deaths:\n")
  print(summary_deaths)
}
#Model 1 :
#Regression of Goal 1 vs total affected: The coefficient for 'total_affected' is -3.41e-08 with a standard error of 4.19e-08. This indicates a very small negative relationship between 'total_affected' and 'Goal 1.' However, the p-value (0.42) is larger than the typical significance level of 0.05, suggesting that the relationship between 'total_affected' and 'Goal 1' is not statistically significant.
#Regression of Goal 1 vs Total Deaths: The coefficient for 'total_deaths' is -0.000117 with a standard error of 0.000165. This indicates a negative relationship between 'total_deaths' and 'Goal 1,' but the p-value (0.32) is larger than 0.05, suggesting that the relationship between 'total_deaths' and 'Goal 1' is not statistically significant.

#Model 2: 
#Regression of Goal 2 vs total affected:The coefficient for 'total_affected' is 3.60e-08 with a standard error of 1.68e-08. This suggests a slight positive relationship between 'total_affected' and 'Goal 2.' The p-value (0.044) is marginally significant, indicating a potential relationship between 'total_affected' and 'Goal 2.'
#Regression of Goal 2 vs Total Deaths: The coefficient for 'total_deaths' is -2.37e-05 with a standard error of 0.000134. This indicates no significant relationship between 'total_deaths' and 'Goal 2,' as the p-value (0.64) is larger than 0.05.

#Model 3: 
#Regression of Goal 3 vs total affected: The coefficient for 'total_affected' is 1.85e-08 with a standard error of 3.72e-08. This shows a very small positive relationship between 'total_affected' and 'Goal 3.' However, the p-value (0.62) is larger than 0.05, suggesting that the relationship between 'total_affected' and 'Goal 3' is not statistically significant.
#Regression of Goal 3 vs Total Deaths: The coefficient for 'total_deaths' is -0.000158 with a standard error of 0.000172. This indicates a negative relationship between 'total_deaths' and 'Goal 3.' The p-value (0.28) is larger than 0.05, suggesting that the relationship between 'total_deaths' and 'Goal 3' is not statistically significant.

#Model 4: 
#Regression of Goal 4 vs total affected: The coefficient for 'total_affected' is -2.07e-09 with a standard error of 1.58e-08. This suggests an extremely small negative relationship between 'total_affected' and 'Goal 4.' Moreover, the p-value (0.89) is much larger than 0.05, indicating no statistically significant relationship between 'total_affected' and 'Goal 4.'
#Regression of Goal 4 vs Total Deaths:The coefficient for 'total_deaths' is 0.000108 with a standard error of 0.000194. This shows a positive relationship between 'total_deaths' and 'Goal 4.' However, the p-value (0.58) is larger than 0.05, suggesting that the relationship between 'total_deaths' and 'Goal 4' is not statistically significant.

#Model 5;
#Regression of Goal 5 vs total affected: The coefficient for 'total_affected' is 2.45e-08 with a standard error of 5.03e-08. This demonstrates a very small positive relationship between 'total_affected' and 'Goal 5.' However, the p-value (0.63) is larger than 0.05, indicating that the relationship between 'total_affected' and 'Goal 5' is not statistically significant.
#Regression of Goal 5 vs Total Deaths: The coefficient for 'total_deaths' is -0.000042 with a standard error of 0.000217. This shows a small negative relationship between 'total_deaths' and 'Goal 5.' Yet, the p-value (0.86) is larger than 0.05, suggesting that the relationship between 'total_deaths' and 'Goal 5' is not statistically significant.

#Model 6:
#Regression of Goal 6 vs total affected:The coefficient for 'total_affected' is -1.02e-08 with a standard error of 1.77e-08. This indicates an extremely small negative relationship between 'total_affected' and 'Goal 6.' Also, the p-value (0.56) is larger than 0.05, suggesting no statistically significant relationship between 'total_affected' and 'Goal 6.'
#Regression of Goal 6 vs Total Deaths: The coefficient for 'total_deaths' is 0.000104 with a standard error of 0.000127. This shows a positive relationship between 'total_deaths' and 'Goal 6.' However, the p-value (0.41) is larger than 0.05, indicating that the relationship between 'total_deaths' and 'Goal 6' is not statistically significant.

#Model 7:
#Regression of Goal 7 vs total affected: The coefficient for 'total_affected' is 5.72e-08 with a standard error of 2.15e-08. This indicates a small positive relationship between 'total_affected' and 'Goal 7.' However, the p-value (0.008) is less than 0.05, suggesting a statistically significant relationship between 'total_affected' and 'Goal 7.'
#Regression of Goal 7 vs Total Deaths: The coefficient for 'total_deaths' is -0.000267 with a standard error of 0.000122. This indicates a moderate negative relationship between 'total_deaths' and 'Goal 7.' Furthermore, the p-value (0.026) is less than 0.05, suggesting a statistically significant relationship between 'total_deaths' and 'Goal 7.'

#Model 8:
#Regression of Goal 7 vs total affected: The coefficient for 'total_affected' is 4.11e-08 with a standard error of 1.09e-08. This indicates a small positive relationship between 'total_affected' and 'Goal 8.' However, the p-value (0.0003) is less than 0.05, indicating a statistically significant relationship between 'total_affected' and 'Goal 8.'
#Regression of Goal 8 vs Total Deaths:The coefficient for 'total_deaths' is -0.000335 with a standard error of 0.000143. This indicates a moderate negative relationship between 'total_deaths' and 'Goal 8.' Additionally, the p-value (0.019) is less than 0.05, suggesting a statistically significant relationship between 'total_deaths' and 'Goal 8.'

#Model 9:
#Regression of Goal 7 vs total affected: The coefficient for 'total_affected' is 1.82e-08 with a standard error of 1.14e-08. This indicates a small positive relationship between 'total_affected' and 'Goal 9.' However, the p-value (0.113) is larger than 0.05, suggesting that the relationship between 'total_affected' and 'Goal 9' is not statistically significant.
#Regression of Goal 9 vs Total Deaths: The coefficient for 'total_deaths' is -0.000212 with a standard error of 0.000176. This indicates a moderate negative relationship between 'total_deaths' and 'Goal 9.' Nevertheless, the p-value (0.231) is larger than 0.05, suggesting that the relationship between 'total_deaths' and 'Goal 9' is not statistically significant.

#Model 10: 
#Regression of Goal 7 vs total affected: The coefficient for 'total_affected' is 3.91e-08 with a standard error of 1.74e-08. This indicates a small positive relationship between 'total_affected' and 'Goal 10.' However, the p-value (0.026) is less than 0.05, suggesting a statistically significant relationship between 'total_affected' and 'Goal 10.'
#Regression of Goal 10 vs Total Deaths: The coefficient for 'total_deaths' is -0.000207 with a standard error of 0.000128. This indicates a moderate negative relationship between 'total_deaths' and 'Goal 10.' Furthermore, the p-value (0.104) is larger than 0.05, indicating that the relationship between 'total_deaths' and 'Goal 10' is not statistically significant.

#Model 11: 
#Regression of Goal 7 vs total affected: The coefficient for 'total_affected' is 6.35e-08 with a standard error of 3.84e-08. This indicates a small positive relationship between 'total_affected' and 'Goal 11.' However, the p-value (0.095) is larger than 0.05, suggesting that the relationship between 'total_affected' and 'Goal 11' is not statistically significant.
#Regression of Goal 11 vs Total Deaths: The coefficient for 'total_deaths' is -0.000097 with a standard error of 0.00016. This indicates a weak negative relationship between 'total_deaths' and 'Goal 11.' Nevertheless, the p-value (0.549) is larger than 0.05, indicating that the relationship between 'total_deaths' and 'Goal 11' is not statistically significant.

#Model 12: 
#Regression of Goal 7 vs total affected:The coefficient for 'total_affected' is 5.28e-08 with a standard error of 2.85e-08. This indicates a small positive relationship between 'total_affected' and 'Goal 12.' However, the p-value (0.069) is larger than 0.05, suggesting that the relationship between 'total_affected' and 'Goal 12' is not statistically significant.
#Regression of Goal 12 vs Total Deaths: The coefficient for 'total_deaths' is -0.000037 with a standard error of 0.000122. This indicates a very weak negative relationship between 'total_deaths' and 'Goal 12.' Moreover, the p-value (0.761) is larger than 0.05, indicating that the relationship between 'total_deaths' and 'Goal 12' is not statistically significant.

#Model 13:
#Regression of Goal 7 vs total affected: The coefficient for 'total_affected' is 1.32e-07 with a standard error of 3.57e-08. This indicates a moderate positive relationship between 'total_affected' and 'Goal 13.' Additionally, the p-value (0.0005) is less than 0.05, suggesting a statistically significant relationship between 'total_affected' and 'Goal 13.'
#Regression of Goal 13 vs Total Deaths:The coefficient for 'total_deaths' is -0.000092 with a standard error of 0.000051. This indicates a moderate negative relationship between 'total_deaths' and 'Goal 13.' Furthermore, the p-value (0.078) is larger than 0.05, indicating that the relationship between 'total_deaths' and 'Goal 13' is not statistically significant.

#Model 14:
#Regression of Goal 7 vs total affected:The coefficient for 'total_affected' is 1.28e-07 with a standard error of 4.12e-08. This indicates a moderate positive relationship between 'total_affected' and 'Goal 14.' However, the p-value (0.002) is less than 0.05, suggesting a statistically significant relationship between 'total_affected' and 'Goal 14.'
#Regression of Goal 14 vs Total Deaths:The coefficient for 'total_deaths' is -0.000042 with a standard error of 0.000048. This indicates a very weak negative relationship between 'total_deaths' and 'Goal 14.' Moreover, the p-value (0.388) is larger than 0.05, indicating that the relationship between 'total_deaths' and 'Goal 14' is not statistically significant.

#Model 15:
#Regression of Goal 7 vs total affected: The coefficient for 'total_affected' is 1.25e-07 with a standard error of 4.27e-08. This indicates a moderate positive relationship between 'total_affected' and 'Goal 15.' However, the p-value (0.003) is less than 0.05, suggesting a statistically significant relationship between 'total_affected' and 'Goal 15.'
#Regression of Goal 15 vs Total Deaths: The coefficient for 'total_deaths' is -0.000073 with a standard error of 0.000057. This indicates a weak negative relationship between 'total_deaths' and 'Goal 15.' Additionally, the p-value (0.205) is larger than 0.05, indicating that the relationship between 'total_deaths' and 'Goal 15' is not statistically significant.

#Model 16:
#Regression of Goal 7 vs total affected: The coefficient for 'total_affected' is 1.85e-07 with a standard error of 1.24e-07. This indicates a moderate positive relationship between 'total_affected' and 'Goal 16.' However, the p-value (0.140) is larger than 0.05, suggesting that the relationship between 'total_affected' and 'Goal 16' is not statistically significant.
#Regression of Goal 16 vs Total Deaths: The coefficient for 'total_deaths' is -0.000093 with a standard error of 0.000131. This indicates a weak negative relationship between 'total_deaths' and 'Goal 16.' Furthermore, the p-value (0.475) is larger than 0.05, indicating that the relationship between 'total_deaths' and 'Goal 16' is not statistically significant.















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






Q3.2 <- read.csv(here("scripts", "data", "data_question3_2.csv"))
covid_filtered <- Q3.2

# Select relevant columns for correlation analysis
relevant_columns <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8",
                      "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16",
                      "stringency", "cases_per_million", "deaths_per_million")

subset_data <- covid_filtered[, relevant_columns]

# Define the specific goal columns you want to include in regression
selected_goals <- c("goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8",
                    "goal9", "goal10", "goal11", "goal12", "goal13", "goal15", "goal16")

# Loop through each selected goal
for (goal in selected_goals) {
  # Formulas for regression against COVID-19 variables
  formula_stringency <- as.formula(paste(goal, "~ stringency"))
  formula_cases <- as.formula(paste(goal, "~ cases_per_million"))
  formula_deaths <- as.formula(paste(goal, "~ deaths_per_million"))
  
  # Perform linear regression for stringency
  lm_stringency <- lm(formula_stringency, data = subset_data)
  lm_cases <- lm(formula_cases, data = subset_data)
  lm_deaths <- lm(formula_deaths, data = subset_data)
  
  
  summary_stringency <- summary(lm_stringency)
  summary_cases <- summary(lm_cases)
  summary_deaths <- summary(lm_deaths)
  
  # Print or save the regression summaries
  cat("\nRegression Summary for", goal, "vs Stringency:\n")
  print(summary_stringency)
  
  cat("\nRegression Summary for", goal, "vs Cases per Million:\n")
  print(summary_cases)
  
  cat("\nRegression Summary for", goal, "vs Deaths per Million:\n")
  print(summary_deaths)
}
### Regression Summary for 'Goal 1' vs Stringency: The regression analysis indicates a statistically significant positive relationship between 'Goal 1' and 'Stringency.' For every unit increase in 'Stringency,' 'Goal 1' tends to increase by 0.1088 units, on average. However, the relationship's explanatory power is limited, as only around 0.25% of the variation in 'Goal 1' can be explained by changes in 'Stringency.' This low adjusted R-squared value suggests that 'Stringency' alone might not be a strong predictor of 'Goal 1.' Other unaccounted-for factors likely contribute to variations in 'Goal 1.'
### Regression Summary for 'Goal 1' vs Cases per Million:The regression analysis shows a statistically significant positive relationship between 'Goal 1' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 1' tends to increase by 1.25e-04 units, on average. The adjusted R-squared value of approximately 1.95% indicates that 'Cases per Million' explains a slightly larger proportion of the variance in 'Goal 1' compared to 'Stringency.' However, similar to 'Stringency,' 'Cases per Million' alone might not comprehensively predict 'Goal 1.'
### Regression Summary for 'Goal 1' vs Deaths per Million:The regression analysis reveals a statistically significant positive relationship between 'Goal 1' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 1' tends to increase by 0.01863 units, on average. The adjusted R-squared value of approximately 2.12% suggests that 'Deaths per Million' explains a slightly higher proportion of the variance in 'Goal 1' compared to both 'Stringency' and 'Cases per Million.' Yet, similar to the other variables, 'Deaths per Million' alone might not fully account for variations in 'Goal 1.'
### Overall Summary:Each of the predictors (Stringency, Cases per Million, Deaths per Million) exhibits statistically significant relationships with 'Goal 1.' However, individually, they explain only a small fraction of the variance in 'Goal 1.' Their limited explanatory power, as indicated by the low adjusted R-squared values (ranging from 0.25% to 2.12%), suggests that other unaccounted-for factors or a combination of variables not included in the analysis might play crucial roles in determining 'Goal 1.' Therefore, while these variables show significance, their individual impacts on 'Goal 1' are modest, and a more comprehensive model incorporating additional factors may better predict 'Goal 1.'


### Regression Summary for 'Goal 2' vs Stringency: The regression analysis shows a statistically significant positive relationship between 'Goal 2' and 'Stringency.' For every unit increase in 'Stringency,' 'Goal 2' tends to increase by 0.0555 units, on average. The t-value (4.77) associated with the coefficient indicates its statistical significance. Additionally, the p-value (1.9e-06) is much smaller than the typical significance level of 0.05, providing strong evidence to reject the null hypothesis. This suggests that the relationship between 'Stringency' and 'Goal 2' is statistically significant. The adjusted R-squared value of approximately 0.00587 implies that only about 0.59% of the variance in 'Goal 2' can be explained by changes in 'Stringency.' Therefore, while statistically significant, 'Stringency' alone might not be a robust predictor of 'Goal 2.'
### Regression Summary for 'Goal 2' vs Cases per Million: The regression analysis indicates a statistically significant positive relationship between 'Goal 2' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 2' tends to increase by 3.10e-05 units, on average. The t-value (6.38) and the associated p-value (2.0e-10) both suggest the statistical significance of this relationship, providing strong evidence to reject the null hypothesis. The adjusted R-squared value of approximately 0.0107 implies that about 1.07% of the variance in 'Goal 2' can be explained by changes in 'Cases per Million.' Similar to 'Stringency,' 'Cases per Million' alone might not comprehensively predict 'Goal 2.'
### Regression Summary for 'Goal 2' vs Deaths per Million: The regression analysis reveals a statistically significant positive relationship between 'Goal 2' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 2' tends to increase by 5.50e-03 units, on average. Both the t-value (7.49) and the associated p-value (8.3e-14) indicate the statistical significance of this relationship. The adjusted R-squared value of approximately 0.0148 implies that about 1.48% of the variance in 'Goal 2' can be explained by changes in 'Deaths per Million.' Similar to the other predictors, 'Deaths per Million' alone might not fully explain variations in 'Goal 2.'
### Overall Summary: Each predictor (Stringency, Cases per Million, Deaths per Million) shows statistically significant relationships with 'Goal 2.' However, individually, they explain only a small portion of the variance in 'Goal 2.' The adjusted R-squared values ranging from approximately 0.59% to 1.48% indicate that these variables alone might not be robust predictors of 'Goal 2.' Other unaccounted-for factors likely contribute to variations in 'Goal 2.' A more comprehensive model incorporating additional variables could better predict 'Goal 2.'


### Regression Summary for 'Goal 3' vs Stringency:The regression analysis reveals a statistically significant positive relationship between 'Goal 3' and 'Stringency.' For every unit increase in 'Stringency,' 'Goal 3' tends to increase by 0.1376 units, on average. The t-value (5.79) associated with the coefficient indicates its statistical significance. Additionally, the p-value (7.7e-09) is much smaller than the typical significance level of 0.05, providing strong evidence to reject the null hypothesis. This suggests that the relationship between 'Stringency' and 'Goal 3' is statistically significant.
#The adjusted R-squared value of approximately 0.00876 implies that only about 0.88% of the variance in 'Goal 3' can be explained by changes in 'Stringency.' Therefore, while statistically significant, 'Stringency' alone might not be a robust predictor of 'Goal 3.'
### Regression Summary for 'Goal 3' vs Cases per Million:The regression analysis indicates a statistically significant positive relationship between 'Goal 3' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 3' tends to increase by 1.07e-04 units, on average. The t-value (10.9) and the associated p-value (less than 2e-16) both suggest the statistical significance of this relationship, providing strong evidence to reject the null hypothesis.
#The adjusted R-squared value of approximately 0.031 implies that about 3.1% of the variance in 'Goal 3' can be explained by changes in 'Cases per Million.' Similar to 'Stringency,' 'Cases per Million' alone might not comprehensively predict 'Goal 3.'
### Regression Summary for 'Goal 3' vs Deaths per Million:The regression analysis shows a statistically significant positive relationship between 'Goal 3' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 3' tends to increase by 0.01522 units, on average. Both the t-value (10.2) and the associated p-value (less than 2e-16) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0272 implies that about 2.72% of the variance in 'Goal 3' can be explained by changes in 'Deaths per Million.' Similar to the other predictors, 'Deaths per Million' alone might not fully explain variations in 'Goal 3.'
### Overall Summary:Each predictor (Stringency, Cases per Million, Deaths per Million) demonstrates statistically significant relationships with 'Goal 3.' However, individually, they explain only a small portion of the variance in 'Goal 3.' The adjusted R-squared values ranging from approximately 0.88% to 3.1% indicate that these variables alone might not be robust predictors of 'Goal 3.' Other unaccounted-for factors likely contribute to variations in 'Goal 3.' A more comprehensive model including additional variables could better predict 'Goal 3.'



### Regression Summary for 'Goal 4' vs Stringency:The regression analysis indicates a statistically significant positive relationship between 'Goal 4' and 'Stringency.' For every unit increase in 'Stringency,' 'Goal 4' tends to increase by 0.1359 units, on average. The t-value (4.81) associated with the coefficient indicates its statistical significance. Additionally, the p-value (1.6e-06) is much smaller than the typical significance level of 0.05, providing strong evidence to reject the null hypothesis. This suggests that the relationship between 'Stringency' and 'Goal 4' is statistically significant.
#The adjusted R-squared value of approximately 0.00597 implies that only about 0.60% of the variance in 'Goal 4' can be explained by changes in 'Stringency.' Therefore, while statistically significant, 'Stringency' alone might not be a robust predictor of 'Goal 4.'
### Regression Summary for 'Goal 4' vs Cases per Million:The regression analysis indicates a statistically significant positive relationship between 'Goal 4' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 4' tends to increase by 9.89e-05 units, on average. The t-value (8.39) and the associated p-value (less than 2e-16) both suggest the statistical significance of this relationship, providing strong evidence to reject the null hypothesis.
#The adjusted R-squared value of approximately 0.0185 implies that about 1.85% of the variance in 'Goal 4' can be explained by changes in 'Cases per Million.' Similar to 'Stringency,' 'Cases per Million' alone might not comprehensively predict 'Goal 4.'
### Regression Summary for 'Goal 4' vs Deaths per Million:The regression analysis reveals a statistically significant positive relationship between 'Goal 4' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 4' tends to increase by 0.01502 units, on average. Both the t-value (8.43) and the associated p-value (less than 2e-16) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0187 implies that about 1.87% of the variance in 'Goal 4' can be explained by changes in 'Deaths per Million.' Similar to the other predictors, 'Deaths per Million' alone might not fully explain variations in 'Goal 4.'
### Overall Summary:Each predictor (Stringency, Cases per Million, Deaths per Million) shows statistically significant relationships with 'Goal 4.' However, individually, they explain only a small portion of the variance in 'Goal 4.' The adjusted R-squared values ranging from approximately 0.60% to 1.87% indicate that these variables alone might not be robust predictors of 'Goal 4.' Other unaccounted-for factors likely contribute to variations in 'Goal 4.' A more comprehensive model including additional variables could better predict 'Goal 4.'



### Regression Summary for 'Goal 5' vs Stringency:The regression analysis reveals a statistically significant positive relationship between 'Goal 5' and 'Stringency.' For every unit increase in 'Stringency,' 'Goal 5' tends to increase by 0.1602 units, on average. The t-value (8.8) associated with the coefficient indicates its statistical significance. Additionally, the p-value (less than 2e-16) is much smaller than the typical significance level of 0.05, providing strong evidence to reject the null hypothesis. This suggests that the relationship between 'Stringency' and 'Goal 5' is statistically significant.
#The adjusted R-squared value of approximately 0.0204 implies that only about 2.04% of the variance in 'Goal 5' can be explained by changes in 'Stringency.' Therefore, while statistically significant, 'Stringency' alone might not be a robust predictor of 'Goal 5.'
### Regression Summary for 'Goal 5' vs Cases per Million:The regression analysis indicates a statistically significant positive relationship between 'Goal 5' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 5' tends to increase by 8.19e-05 units, on average. The t-value (10.8) and the associated p-value (less than 2e-16) both suggest the statistical significance of this relationship, providing strong evidence to reject the null hypothesis.
#The adjusted R-squared value of approximately 0.0304 implies that about 3.04% of the variance in 'Goal 5' can be explained by changes in 'Cases per Million.' Similar to 'Stringency,' 'Cases per Million' alone might not comprehensively predict 'Goal 5.'
### Regression Summary for 'Goal 5' vs Deaths per Million:The regression analysis shows a statistically significant positive relationship between 'Goal 5' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 5' tends to increase by 0.01198 units, on average. Both the t-value (10.4) and the associated p-value (less than 2e-16) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0284 implies that about 2.84% of the variance in 'Goal 5' can be explained by changes in 'Deaths per Million.' Similar to the other predictors, 'Deaths per Million' alone might not fully explain variations in 'Goal 5.'
### Overall Summary:Each predictor (Stringency, Cases per Million, Deaths per Million) demonstrates statistically significant relationships with 'Goal 5.' However, individually, they explain only a small portion of the variance in 'Goal 5.' The adjusted R-squared values ranging from approximately 2.04% to 3.04% indicate that these variables alone might not be robust predictors of 'Goal 5.' Other unaccounted-for factors likely contribute to variations in 'Goal 5.' A more comprehensive model including additional variables could better predict 'Goal 5.'



### Regression Summary for 'Goal 6' vs Stringency:The regression analysis shows a statistically significant positive relationship between 'Goal 6' and 'Stringency.' For every unit increase in 'Stringency,' 'Goal 6' tends to increase by 0.0586 units, on average. The t-value (3.7) associated with the coefficient indicates its statistical significance. Additionally, the p-value (0.000221) is much smaller than the typical significance level of 0.05, providing strong evidence to reject the null hypothesis. This suggests that the relationship between 'Stringency' and 'Goal 6' is statistically significant.
#The adjusted R-squared value of approximately 0.00343 implies that only about 0.343% of the variance in 'Goal 6' can be explained by changes in 'Stringency.' Therefore, while statistically significant, 'Stringency' alone might not be a robust predictor of 'Goal 6.'
### Regression Summary for 'Goal 6' vs Cases per Million:The regression analysis indicates a statistically significant positive relationship between 'Goal 6' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 6' tends to increase by 6.08e-05 units, on average. The t-value (9.25) and the associated p-value (less than 2e-16) both suggest the statistical significance of this relationship, providing strong evidence to reject the null hypothesis.
#The adjusted R-squared value of approximately 0.0225 implies that about 2.25% of the variance in 'Goal 6' can be explained by changes in 'Cases per Million.' Similar to 'Stringency,' 'Cases per Million' alone might not comprehensively predict 'Goal 6.'
### Regression Summary for 'Goal 6' vs Deaths per Million:The regression analysis shows a statistically significant positive relationship between 'Goal 6' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 6' tends to increase by 9.99e-03 units, on average. Both the t-value (10.1) and the associated p-value (less than 2e-16) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0265 implies that about 2.65% of the variance in 'Goal 6' can be explained by changes in 'Deaths per Million.' Similar to the other predictors, 'Deaths per Million' alone might not fully explain variations in 'Goal 6.'
### Overall Summary:Each predictor (Stringency, Cases per Million, Deaths per Million) demonstrates statistically significant relationships with 'Goal 6.' However, individually, they explain only a small portion of the variance in 'Goal 6.' The adjusted R-squared values ranging from approximately 0.343% to 2.65% indicate that these variables alone might not be robust predictors of 'Goal 6.' Other unaccounted-for factors likely contribute to variations in 'Goal 6.' A more comprehensive model including additional variables could better predict 'Goal 6.'



### Regression Summary for 'Goal 7' vs Stringency:The regression analysis shows a statistically significant positive relationship between 'Goal 7' and 'Stringency.' For every unit increase in 'Stringency,' 'Goal 7' tends to increase by 0.1052 units, on average. The t-value (4.62) associated with the coefficient indicates its statistical significance. Additionally, the p-value (3.91e-06) is much smaller than the typical significance level of 0.05, providing strong evidence to reject the null hypothesis. This suggests that the relationship between 'Stringency' and 'Goal 7' is statistically significant.
#The adjusted R-squared value of approximately 0.00551 implies that only about 0.551% of the variance in 'Goal 7' can be explained by changes in 'Stringency.' Therefore, while statistically significant, 'Stringency' alone might not be a robust predictor of 'Goal 7.'
### Regression Summary for 'Goal 7' vs Cases per Million:The regression analysis indicates a statistically significant positive relationship between 'Goal 7' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 7' tends to increase by 8.39e-05 units, on average. The t-value (8.87) and the associated p-value (less than 2e-16) both suggest the statistical significance of this relationship, providing strong evidence to reject the null hypothesis.
#The adjusted R-squared value of approximately 0.0207 implies that about 2.07% of the variance in 'Goal 7' can be explained by changes in 'Cases per Million.' Similar to 'Stringency,' 'Cases per Million' alone might not comprehensively predict 'Goal 7.'
### Regression Summary for 'Goal 7' vs Deaths per Million:The regression analysis shows a statistically significant positive relationship between 'Goal 7' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 7' tends to increase by 0.01387 units, on average. Both the t-value (9.71) and the associated p-value (less than 2e-16) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0247 implies that about 2.47% of the variance in 'Goal 7' can be explained by changes in 'Deaths per Million.' Similar to the other predictors, 'Deaths per Million' alone might not fully explain variations in 'Goal 7.'
### Overall Summary:Each predictor (Stringency, Cases per Million, Deaths per Million) demonstrates statistically significant relationships with 'Goal 7.' However, individually, they explain only a small portion of the variance in 'Goal 7.' The adjusted R-squared values ranging from approximately 0.551% to 2.47% indicate that these variables alone might not be robust predictors of 'Goal 7.' Other unaccounted-for factors likely contribute to variations in 'Goal 7.' A more comprehensive model including additional variables could better predict 'Goal 7.'


### Regression Summary for 'Goal 8' vs Stringency:The regression analysis reveals a statistically significant positive relationship between 'Goal 8' and 'Stringency.' For every unit increase in 'Stringency,' 'Goal 8' tends to increase by 0.0336 units, on average. The associated t-value (3.23) and the small p-value (0.0013) both indicate the statistical significance of this relationship, providing evidence to reject the null hypothesis. 
#However, the adjusted R-squared value of approximately 0.00256 implies that only about 0.256% of the variance in 'Goal 8' can be explained by changes in 'Stringency.' This suggests that while statistically significant, 'Stringency' alone might not be a robust predictor of 'Goal 8.'
### Regression Summary for 'Goal 8' vs Cases per Million:The regression analysis demonstrates a statistically significant positive relationship between 'Goal 8' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 8' tends to increase by 4.15e-05 units, on average. Both the t-value (9.62) and the extremely small p-value (much less than 0.001) indicate the statistical significance of this relationship, providing strong evidence to reject the null hypothesis.
#The adjusted R-squared value of approximately 0.0243 implies that about 2.43% of the variance in 'Goal 8' can be explained by changes in 'Cases per Million.' Similar to 'Stringency,' 'Cases per Million' alone might not comprehensively predict 'Goal 8.'
### Regression Summary for 'Goal 8' vs Deaths per Million:The regression analysis indicates a statistically significant positive relationship between 'Goal 8' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 8' tends to increase by 5.43e-03 units, on average. Both the t-value (8.28) and the associated p-value (much less than 0.001) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0181 implies that about 1.81% of the variance in 'Goal 8' can be explained by changes in 'Deaths per Million.' Similar to the other predictors, 'Deaths per Million' alone might not fully explain variations in 'Goal 8.'
### Overall Summary:Each predictor (Stringency, Cases per Million, Deaths per Million) demonstrates statistically significant relationships with 'Goal 8.' However, individually, they explain only a small portion of the variance in 'Goal 8.' The adjusted R-squared values ranging from approximately 0.256% to 2.43% indicate that these variables alone might not be robust predictors of 'Goal 8.' Other unaccounted-for factors likely contribute to variations in 'Goal 8.' A more comprehensive model including additional variables could better predict 'Goal 8.'



### Regression Summary for 'Goal 9' vs Stringency: The regression analysis indicates a statistically significant positive relationship between 'Goal 9' and 'Stringency.' For every unit increase in 'Stringency,' 'Goal 9' tends to increase by 0.3291 units, on average. Both the t-value (12.2) and the extremely small p-value (much less than 0.001) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0388 implies that about 3.88% of the variance in 'Goal 9' can be explained by changes in 'Stringency.' Although statistically significant, 'Stringency' alone might not be a comprehensive predictor of 'Goal 9.'
### Regression Summary for 'Goal 9' vs Cases per Million:The regression analysis reveals a statistically significant positive relationship between 'Goal 9' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 9' tends to increase by 1.85e-04 units, on average. Both the t-value (16.7) and the associated p-value (much less than 0.001) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0699 implies that about 6.99% of the variance in 'Goal 9' can be explained by changes in 'Cases per Million.' Similar to 'Stringency,' 'Cases per Million' might not be a comprehensive predictor of 'Goal 9.'
### Regression Summary for 'Goal 9' vs Deaths per Million:The regression analysis indicates a statistically significant positive relationship between 'Goal 9' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 9' tends to increase by 0.0249 units, on average. Both the t-value (14.7) and the extremely small p-value (much less than 0.001) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0553 implies that about 5.53% of the variance in 'Goal 9' can be explained by changes in 'Deaths per Million.' However, similar to the other predictors, 'Deaths per Million' alone might not be a comprehensive predictor of 'Goal 9.'
### Overall Summary:#Each predictor (Stringency, Cases per Million, Deaths per Million) demonstrates statistically significant relationships with 'Goal 9.' However, individually, they explain only a small portion of the variance in 'Goal 9.' The adjusted R-squared values ranging from approximately 3.88% to 6.99% indicate that these variables alone might not comprehensively predict 'Goal 9.' Other unaccounted-for factors likely contribute to variations in 'Goal 9.' A more comprehensive model including additional variables could better predict 'Goal 9.'



### Regression Summary for 'Goal 10' vs Stringency:The regression analysis shows a statistically significant positive relationship between 'Goal 10' and 'Stringency.' For every unit increase in 'Stringency,' 'Goal 10' tends to increase by 0.0746 units, on average. The t-value (2.37) and associated p-value (0.018) indicate that this relationship is statistically significant at a moderate level.
#The adjusted R-squared value of approximately 0.00141 implies that only about 0.141% of the variance in 'Goal 10' can be explained by changes in 'Stringency.' It suggests that 'Stringency' alone might not be a robust predictor of 'Goal 10.'
### Regression Summary for 'Goal 10' vs Cases per Million:The regression analysis indicates a statistically significant positive relationship between 'Goal 10' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 10' tends to increase by 8.90e-05 units, on average. Both the t-value (6.78) and the extremely small p-value (1.4e-11) suggest the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0135 implies that about 1.35% of the variance in 'Goal 10' can be explained by changes in 'Cases per Million.' This suggests that while statistically significant, 'Cases per Million' might not be a comprehensive predictor of 'Goal 10.'
### Regression Summary for 'Goal 10' vs Deaths per Million:The regression analysis reveals a statistically significant positive relationship between 'Goal 10' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 10' tends to increase by 0.00817 units, on average. The t-value (4.24) and the extremely small p-value (2.3e-05) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.00513 implies that about 0.513% of the variance in 'Goal 10' can be explained by changes in 'Deaths per Million.' However, similar to the other predictors, 'Deaths per Million' alone might not be a comprehensive predictor of 'Goal 10.'
### Overall Summary:Each predictor (Stringency, Cases per Million, Deaths per Million) shows statistically significant relationships with 'Goal 10.' However, individually, they explain only a small portion of the variance in 'Goal 10.' The adjusted R-squared values ranging from approximately 0.141% to 1.35% indicate that these variables alone might not comprehensively predict 'Goal 10.' Other unaccounted-for factors likely contribute to variations in 'Goal 10.' A more comprehensive model including additional variables could better predict 'Goal 10.'


### Regression Summary for 'Goal 11' vs Stringency:The regression analysis indicates a statistically significant positive relationship between 'Goal 11' and 'Stringency.' For every unit increase in 'Stringency,' 'Goal 11' tends to increase by 0.0592 units, on average. The t-value (3.05) and associated p-value (0.0023) indicate that this relationship is statistically significant at a moderate level.
#The adjusted R-squared value of approximately 0.00225 suggests that only about 0.225% of the variance in 'Goal 11' can be explained by changes in 'Stringency.' It implies that 'Stringency' alone might not be a robust predictor of 'Goal 11.'
### Regression Summary for 'Goal 11' vs Cases per Million:The regression analysis shows a statistically significant positive relationship between 'Goal 11' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 11' tends to increase by 7.13e-05 units, on average. Both the t-value (8.83) and the extremely small p-value (<2e-16) suggest the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0205 implies that about 2.05% of the variance in 'Goal 11' can be explained by changes in 'Cases per Million.' This suggests that while statistically significant, 'Cases per Million' might not be a comprehensive predictor of 'Goal 11.'
### Regression Summary for 'Goal 11' vs Deaths per Million:The regression analysis reveals a statistically significant positive relationship between 'Goal 11' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 11' tends to increase by 0.01061 units, on average. The t-value (8.68) and the extremely small p-value (<2e-16) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0198 suggests that about 1.98% of the variance in 'Goal 11' can be explained by changes in 'Deaths per Million.' However, similar to the other predictors, 'Deaths per Million' alone might not be a comprehensive predictor of 'Goal 11.'
### Overall Summary:Each predictor (Stringency, Cases per Million, Deaths per Million) shows statistically significant relationships with 'Goal 11.' However, individually, they explain only a small portion of the variance in 'Goal 11.' The adjusted R-squared values ranging from approximately 0.225% to 2.05% indicate that these variables alone might not comprehensively predict 'Goal 11.' Other unaccounted-for factors likely contribute to variations in 'Goal 11.' A more comprehensive model including additional variables could better predict 'Goal 11.'


### Regression Summary for 'Goal 12' vs Stringency:The regression analysis indicates that there is no statistically significant relationship between 'Goal 12' and 'Stringency.' The coefficient for 'Stringency' is 0.00975, and the associated p-value (0.59) is much higher than the commonly accepted significance level of 0.05. This result suggests that there is insufficient evidence to conclude that 'Stringency' significantly predicts 'Goal 12.'
#The adjusted R-squared value being close to zero (-0.000191) suggests that 'Stringency' does not explain any noticeable variance in 'Goal 12.' Therefore, 'Stringency' might not be a relevant predictor for 'Goal 12' based on this analysis.
### Regression Summary for 'Goal 12' vs Cases per Million:The regression analysis indicates a statistically significant negative relationship between 'Goal 12' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 12' tends to decrease by 7.19e-05 units, on average. Both the t-value (-9.71) and the extremely small p-value (<2e-16) suggest the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0247 implies that about 2.47% of the variance in 'Goal 12' can be explained by changes in 'Cases per Million.' However, it's important to note that even though this relationship is statistically significant, 'Cases per Million' alone might not comprehensively predict 'Goal 12.'
### Regression Summary for 'Goal 12' vs Deaths per Million:The regression analysis reveals a statistically significant negative relationship between 'Goal 12' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 12' tends to decrease by 0.00611 units, on average. The t-value (-5.4) and the very small p-value (<2e-16) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0076 suggests that about 0.76% of the variance in 'Goal 12' can be explained by changes in 'Deaths per Million.' Similar to the other predictors, 'Deaths per Million' alone might not be a comprehensive predictor of 'Goal 12.'
### Overall Summary:Among the predictors ('Stringency,' 'Cases per Million,' 'Deaths per Million'), only 'Cases per Million' and 'Deaths per Million' show statistically significant relationships with 'Goal 12.' However, individually, they explain only a small portion of the variance in 'Goal 12.' Other unaccounted-for factors are likely contributing to variations in 'Goal 12.' A more comprehensive model including additional variables could better predict 'Goal 12.'


### Regression Summary for 'Goal 13' vs Stringency:The regression analysis suggests that there is no statistically significant relationship between 'Goal 13' and 'Stringency.' The coefficient for 'Stringency' is 0.0110, and the associated p-value (0.63) is notably higher than the commonly accepted significance level of 0.05. This result suggests that there is insufficient evidence to conclude that 'Stringency' significantly predicts 'Goal 13.'
#The adjusted R-squared value being close to zero (-0.00021) indicates that 'Stringency' does not explain any substantial variance in 'Goal 13.' Therefore, 'Stringency' might not be a relevant predictor for 'Goal 13' based on this analysis.
### Regression Summary for 'Goal 13' vs Cases per Million:The regression analysis indicates a statistically significant negative relationship between 'Goal 13' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 13' tends to decrease by 6.47e-05 units, on average. Both the t-value (-6.74) and the extremely small p-value (<2e-16) suggest the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0119 implies that about 1.19% of the variance in 'Goal 13' can be explained by changes in 'Cases per Million.' However, it's important to note that even though this relationship is statistically significant, 'Cases per Million' alone might not comprehensively predict 'Goal 13.'
### Regression Summary for 'Goal 13' vs Deaths per Million:The regression analysis suggests that there might be a marginal relationship between 'Goal 13' and 'Deaths per Million,' but it is not statistically significant at the conventional significance level of 0.05. The coefficient for 'Deaths per Million' is -0.00277, and the associated p-value (0.059) is slightly higher than 0.05.
#The adjusted R-squared value of approximately 0.000701 implies that less than 1% of the variance in 'Goal 13' can be explained by changes in 'Deaths per Million.' This predictor alone might not sufficiently predict 'Goal 13' based on the observed data.
### Overall Summary:Among the predictors ('Stringency,' 'Cases per Million,' 'Deaths per Million'), only 'Cases per Million' shows a statistically significant relationship with 'Goal 13.' However, it explains only a small portion of the variance in 'Goal 13.' Other unaccounted-for factors are likely contributing to variations in 'Goal 13.' A more comprehensive model including additional variables could better predict 'Goal 13.'



### Regression Summary for 'Goal 15' vs Stringency: The regression analysis indicates that there's no statistically significant relationship between 'Goal 15' and 'Stringency.' The coefficient for 'Stringency' is 0.00953, and the associated p-value (0.5) is notably higher than the commonly accepted significance level of 0.05. This suggests that there isn't enough evidence to conclude that 'Stringency' significantly predicts 'Goal 15.'
#The adjusted R-squared value being close to zero (-0.000151) indicates that 'Stringency' does not explain any substantial variance in 'Goal 15.' Therefore, 'Stringency' might not be a relevant predictor for 'Goal 15' based on this analysis.
### Regression Summary for 'Goal 15' vs Cases per Million:The regression analysis shows a statistically significant but relatively weak positive relationship between 'Goal 15' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 15' tends to increase by 2.45e-05 units, on average. Both the t-value (4.1) and the small p-value (4.3e-05) suggest the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.00427 implies that about 0.427% of the variance in 'Goal 15' can be explained by changes in 'Cases per Million.' However, even though this relationship is statistically significant, 'Cases per Million' alone might not comprehensively predict 'Goal 15.'
### Regression Summary for 'Goal 15' vs Deaths per Million:The regression analysis suggests a statistically significant positive relationship between 'Goal 15' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 15' tends to increase by 0.00574 units, on average. Both the t-value (6.37) and the very small p-value (2.1e-10) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0107 implies that about 1.07% of the variance in 'Goal 15' can be explained by changes in 'Deaths per Million.' However, it's important to note that while this relationship is statistically significant, 'Deaths per Million' alone might not sufficiently predict 'Goal 15.'
### Overall Summary:Among the predictors ('Stringency,' 'Cases per Million,' 'Deaths per Million'), 'Cases per Million' and 'Deaths per Million' show statistically significant relationships with 'Goal 15.' However, both predictors individually explain only a small portion of the variance in 'Goal 15.' Other unaccounted-for factors are likely contributing to variations in 'Goal 15.' A more comprehensive model including additional variables could better predict 'Goal 15.'



### Regression Summary for 'Goal 16' vs Stringency:The regression analysis indicates that there's no statistically significant relationship between 'Goal 16' and 'Stringency.' The coefficient for 'Stringency' is 0.00461, and the associated p-value (0.77) is notably higher than the commonly accepted significance level of 0.05. This suggests that there isn't enough evidence to conclude that 'Stringency' significantly predicts 'Goal 16.'
#The adjusted R-squared value being close to zero (-0.000248) indicates that 'Stringency' does not explain any substantial variance in 'Goal 16.' Therefore, 'Stringency' might not be a relevant predictor for 'Goal 16' based on this analysis.
### Regression Summary for 'Goal 16' vs Cases per Million:The regression analysis shows a statistically significant but relatively weak positive relationship between 'Goal 16' and 'Cases per Million.' For every unit increase in 'Cases per Million,' 'Goal 16' tends to increase by 6.19e-05 units, on average. Both the t-value (9.6) and the small p-value (<2e-16) suggest the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0242 implies that about 2.42% of the variance in 'Goal 16' can be explained by changes in 'Cases per Million.' However, even though this relationship is statistically significant, 'Cases per Million' alone might not comprehensively predict 'Goal 16.'
### Regression Summary for 'Goal 16' vs Deaths per Million:The regression analysis suggests a statistically significant positive relationship between 'Goal 16' and 'Deaths per Million.' For every unit increase in 'Deaths per Million,' 'Goal 16' tends to increase by 6.37e-03 units, on average. Both the t-value (6.49) and the very small p-value (<2e-16) indicate the statistical significance of this relationship.
#The adjusted R-squared value of approximately 0.0111 implies that about 1.11% of the variance in 'Goal 16' can be explained by changes in 'Deaths per Million.' However, while this relationship is statistically significant, 'Deaths per Million' alone might not sufficiently predict 'Goal 16.'
### Overall Summary:Among the predictors ('Stringency,' 'Cases per Million,' 'Deaths per Million'), 'Cases per Million' and 'Deaths per Million' show statistically significant relationships with 'Goal 16.' However, both predictors individually explain only a small portion of the variance in 'Goal 16.' Other unaccounted-for factors are likely contributing to variations in 'Goal 16.' A more comprehensive model including additional variables could better predict 'Goal 16.'




### General Overview:
#- For each goal (Goal 1 to Goal 9), multiple predictors (Stringency, Cases per Million, Deaths per Million) exhibit statistically significant relationships.
#- However, individually, these predictors explain only a small fraction of the variance in each goal, ranging from 0.25% to 6.99%.
#- The adjusted R-squared values for these relationships indicate limited explanatory power, suggesting that other unaccounted-for factors significantly influence the variation in each goal.
#- Each goal's prediction based solely on the given predictors (Stringency, Cases per Million, Deaths per Million) is modest.
#- To enhance prediction accuracy for each goal, a more comprehensive model that incorporates additional variables or unexplored factors is recommended.

#In summary, while Stringency, Cases per Million, and Deaths per Million show statistically significant relationships with each goal, their individual impacts are limited in explaining the variations observed. Therefore, incorporating further variables or exploring additional factors beyond the current predictors might significantly improve the predictive capability for each respective goal.



#Certainly, here's an amalgamated overall summary:

### Overall Summary:
#- Across all goals (from Goal 1 to Goal 16), the predictors (Stringency, Cases per Million, and Deaths per Million) exhibit statistically significant relationships.
#- However, when individually assessed, these predictors explain only a marginal fraction of the variance in the respective goals, with explanatory percentages ranging from approximately 0.141% to 6.99%.
#- The adjusted R-squared values consistently indicate limited explanatory power for these relationships, implying the influence of other unaccounted-for factors in driving the variations observed in each goal.
#- Solely relying on Stringency, Cases per Million, and Deaths per Million results in modest predictive capabilities for each goal.
#- To significantly enhance predictive accuracy for each goal, it is recommended to develop more comprehensive models that encompass additional variables or explore unexplored factors beyond the current predictors.

### In Summary: The statistical significance of Stringency, Cases per Million, and Deaths per Million in relation to each goal is evident. However, these predictors individually fall short in explaining the observed variations, emphasizing the necessity of exploring additional variables or unexplored factors to meaningfully enhance predictive capability for each respective goal.



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
