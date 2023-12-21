
#### Distribution of Goals  per years ####

As we have seen previously, our Goals are not normally ditributed.
First, let's look at how our goals are distributed. To do that, it is important to show the distribution at a year level since showing it a once would be impacted by the change over the year. We created below a shiny app that allows us to select the year we want to look at and see the distribution of the goals for that year. Since shiny apps are not well supported with quarto, we show here only scrrenshots. We chose to show year 2000 and year 2023 to see both extreme in terms of distribution. it is interesting to see how the goals tended to be skewed in the past, well representing the developing countries lagging behind. But the ditribution beeing more normaly ditributed in recent years.


data <- read.csv(here("scripts","data","data_question24.csv"))

data_long <- data %>% 
  gather(key = "goal", value = "score", goal1:goal17) %>%
  mutate(goal = factor(goal, levels = paste0("goal", 1:17)))

ui <- fluidPage(
  titlePanel("Goals Score Distribution"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Year", choices = unique(data_long$year))
    ),
    mainPanel(
      plotOutput("ridgePlot")
    )
  )
)

server <- function(input, output) {
  output$ridgePlot <- renderPlot({
    filteredData <- filter(data_long, year == input$year)
    ggplot(filteredData, aes(x = score, y = goal, fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
      geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
      scale_fill_viridis_c(name = "Value", option = "C") +
      labs(title = paste("Goal Score Distribution in", input$year))
  })
}

shinyApp(ui = ui, server = server)


#### Forward selection "graph going down" (AIC/R^2) ####

library(MASS)
clean_data <- na.omit(data_4)
# Initialize variables to store the results
step_results <- data.frame(step = integer(), aic = numeric(), adjusted_r_squared = numeric())

# Initial model (null model)
current_model <- lm(overallscore ~ 1, data = clean_data)

# Record initial metrics
step_results <- rbind(step_results, data.frame(step = 0, aic = AIC(current_model), adjusted_r_squared = summary(current_model)$adj.r.squared))

# Perform forward selection
for (variable in colnames(clean_data)[grepl("goal", colnames(clean_data))]) {
  current_model <- update(current_model, paste(". ~ . +", variable))
  current_step <- nrow(step_results) + 1
  step_results <- rbind(step_results, data.frame(step = current_step, aic = AIC(current_model), adjusted_r_squared = summary(current_model)$adj.r.squared))
}

ggplot(step_results, aes(x = step)) +
  geom_line(aes(y = aic, color = "AIC")) +
  geom_line(aes(y = adjusted_r_squared * 100, color = "Adjusted R-squared")) +
  labs(title = "Forward Selection Process", x = "Step", y = "Metric Value") +
  scale_color_manual("", breaks = c("AIC", "Adjusted R-squared"), values = c("blue", "red"))



##############################################################################



install.packages("plotly")
install.packages("htmlwidgets")

library(plotly)
library(htmlwidgets)

set.seed(123)
df1 <- data.frame(x = rnorm(100), y = rnorm(100))
df2 <- data.frame(x = rnorm(100, 1), y = rnorm(100, 2))


p1 <- plot_ly(df1, x = ~x, y = ~y, type = 'scatter', mode = 'markers')
p2 <- plot_ly(df2, x = ~x, y = ~y, type = 'scatter', mode = 'markers')

saveWidget(p1, "plot1.html", selfcontained = TRUE)
saveWidget(p2, "plot2.html", selfcontained = TRUE)

<!DOCTYPE html>
<html>
<head>
  <title>Interactive Plots</title>
</head>
<body>
  <button onclick="showPlot('plot1.html')">Show Plot 1</button>
  <button onclick="showPlot('plot2.html')">Show Plot 2</button>
  
  <div id="plot"></div>
  
  <script>
    function showPlot(plotFile) {
      var xhttp = new XMLHttpRequest();
      xhttp.onreadystatechange = function() {
        if (this.readyState == 4 && this.status == 200) {
          document.getElementById("plot").innerHTML = this.responseText;
        }
      };
      xhttp.open("GET", plotFile, true);
      xhttp.send();
    }
  </script>
</body>
</html>
  
  
  
  
set.seed(123)
df1 <- data.frame(x = rnorm(100), y = rnorm(100))
df2 <- data.frame(x = rnorm(100, 1), y = rnorm(100, 2))

p1 <- plot_ly(df1, x = ~x, y = ~y, type = 'scatter', mode = 'markers')
p2 <- plot_ly(df2, x = ~x, y = ~y, type = 'scatter', mode = 'markers')


  
selectInput("graphType", "Choose Graph:", choices = c("Graph 1", "Graph 2"))

output$plot <- renderPlotly({
  if(input$graphType == "Graph 1") {
    p1
  } else {
    p2
  }
})

plotlyOutput("plot")

# # Drop unnecessary columns
# 
# data <- dplyr::select(data_question24, -c(code))
# 
# # Reshape the data to long format using pivot_longer
# long_df <- pivot_longer(data, 
#                         cols = starts_with("goal"), 
#                         names_to = "Goal", 
#                         values_to = "Value")
# 
# ui <- fluidPage(
#   titlePanel("Interactive Goal Distribution"),
#   selectInput("year", "Select Year:", choices = unique(long_df$year)),
#   plotlyOutput("distPlot")
# )
# 
# server <- function(input, output) {
#   output$distPlot <- renderPlotly({
#     # Convert input year to numeric if necessary
#     selected_year <- as.numeric(input$year)
#     
#     # Filter the data based on selected year
#     filtered_data <- long_df[long_df$year == selected_year, ]
#     
#     
#     # Create the ggplot
#     p <- ggplot(filtered_data, aes(x = Value, y = Goal, fill = stat(x))) +
#       geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
#       scale_fill_viridis_c(name = "Value", option = "C") +
#       labs(title = 'Distribution of Goals')
#     
#     # Convert to plotly object
#     ggplotly(p)
#   })
# }
# 
# shinyApp(ui, server)
# summary(long_df)
# 
# ####
# 
# data <- data_question24
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# ui <- fluidPage(
#   titlePanel("Goals Distribution Map"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("region", "Select Region", choices = unique(data$region)),
#       selectInput("year", "Select Year", choices = unique(data$year))
#     ),
#     mainPanel(leafletOutput("map"))
#   )
# )
# 
# # Define a color palette
# pal <- colorNumeric(palette = "viridis", domain = data$overallscore)
# 
# 
# server <- function(input, output) {
#   filteredData <- reactive({
#     data %>% 
#       filter(region == input$region, year == input$year)
#   })
#   
#   output$map <- renderLeaflet({
#     data <- filteredData()
#     mapData <- merge(world, data, by.x = "name", by.y = "country")
#     
#     leaflet(mapData) %>%
#       addTiles() %>%
#       addPolygons(fillColor = ~pal(overallscore), 
#                   fillOpacity = 0.7, 
#                   weight = 1, 
#                   color = "white")
#   })
# }
# 
# shinyApp(ui = ui, server = server)


```{r echo = TRUE, message = FALSE}
#### Preparation of the data ####

# Keeping only the columns of interest for the correlation calculation
data_4_goals <- data_4 %>%
  dplyr::select(overallscore, goal1, goal2, goal3, goal4, goal5,
                goal6,goal7, goal8, goal9, goal10, goal11, goal12,
                goal13, goal15, goal16, goal17)
```

```{r echo = TRUE, message = FALSE}
#### Spearman Correlation ####

# Calculate the correlation between our variables
spearman_corr_4 <-cor(data_4_goals, # using only the columns of interest
                      method = "spearman", # using the Spearman correlation
                      use = "everything") # using all the data

# Apply threshold and replace values below it with NAs
spearman_corr_4[abs(spearman_corr_4) < threshold_heatmap] <- NA
```

```{r echo = TRUE, message = FALSE}
#### Spearman Correlation Heatmap ####

# Melting the data
melted_corr_4 <- melt(spearman_corr_4, na.rm = TRUE)

# Creation of the heatmap
ggplot(data = melted_corr_4, # melted data
       aes(x = Var1, # x axis
           y = Var2, # y axis
           fill = value)) +
    geom_tile() + # Adding tiles
    geom_text(aes(label = sprintf("%.2f", value)), # Adding the correlation values
              vjust = 0.5,
              size=2.5) +
    scale_fill_viridis_c(name = "Spearman\nCorrelation", # Adding the color legend
                         na.value = "white", # Setting the color for NAs
                         begin = 0.1, # changing the color to prevent dark colors
                         end = 1) +
    theme_minimal() + # applying a minimal theme to be consistant with our other graphs
    theme(axis.text.x = element_text(angle = 45, # Rotating the x axis labels 45 degrees
                                     hjust = 1)) +
    labs(title = "Heatmap of Spearman Correlations for Goals",  # Adding a title to our graph
         x = "",
         y = "")
```

