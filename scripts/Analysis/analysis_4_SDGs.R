
#### Distribution of Goals  per years ####

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

