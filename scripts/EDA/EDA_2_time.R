##### How has the adoption of the SDGs in 2015 influenced the achievement of SDGs? #####

data_question2 <- read.csv(here("scripts", "data", "data_question24.csv"), sep=",")

### Big map

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(shiny)
library(ggpointdensity)
library(plotly)
library(shinyjs)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge data with the world map data
data0 <- merge(world, data_question2, by.x = "iso_a3", by.y = "code", all.x = TRUE)

data0 %>%
  st_transform(crs="+proj=robin") %>%
  ggplot() +
  geom_sf(color="lightgrey") +
  geom_sf(aes(fill=overallscore)) +
  scale_fill_gradient(low = "red", high = "green", na.value = "grey50", name = "Overall Score") +
  coord_sf(datum=NA) +
  theme_minimal()

# The same but interactive

# Define UI logic
ui <- function(request) {
  fluidPage(
    titlePanel("Interactive Map with Overall Score"),
    plotlyOutput("map"),
    sliderInput("year", "Select Year:", min = 2000, max = 2022, value = 2022, step = 1, width = "100%"),
    textOutput("hovered_country_text")
  )
}

# Define server logic
server <- function(input, output, session) {
  # Filter data based on the selected year
  selected_data <- reactive({
    filter(data0, year == as.numeric(input$year))
  })
  
  # Create a plotly object with hover information
  output$map <- renderPlotly({
    plot_ly(
      type = "choropleth",
      z = ~selected_data()$overallscore,
      locations = ~selected_data()$iso_a3,
      text = ~paste("Country: ", selected_data()$name, "<br>Overall Score: ", selected_data()$overallscore),
      colors = c("darkred", "orange", "yellow", "darkgreen"),
      colorbar = list(title = "Overall Score"),
      hoverinfo = "text"
    )
  })
  
  # Display the name and overall score of the hovered country
  output$hovered_country_text <- renderText({
    req(input$hovered_country)
    country_data <- data0[data0$iso_a3 == input$hovered_country, ]
    paste("Country: ", country_data$name, ", Overall Score: ", country_data$overallscore)
  })
}

# Run the Shiny app
shinyApp(ui, server)

### First, look at the evolution of SDG achievement overall score over time

# Evolution over time of the overall score: mean over all countries

data1 <- data_question2 %>% group_by(year) %>%
  mutate(mean_overall_score_by_year=mean(overallscore))
ggplot(data=data1) +
  geom_line(mapping=aes(x=year, y=mean_overall_score_by_year))

# Evolution over time of the overall score: mean by continent

data2 <- data_question2 %>% group_by(year, continent) %>%
  mutate(mean_overall_score_by_year=mean(overallscore))
ggplot(data=data2) +
  geom_line(mapping=aes(x=year, y=mean_overall_score_by_year, color=continent), lwd=1)

# Evolution over time of the overall score: mean by region

data3 <- data_question2 %>% group_by(year, region) %>%
  mutate(mean_overall_score_by_year=mean(overallscore))
ggplot(data=data3) +
  geom_line(mapping=aes(x=year, y=mean_overall_score_by_year, color=region), lwd=1)

### Second, look at the evolution of SDG achievement scores (16) over time

# Evolution over time of the SDGs' achievement: mean over all countries

data4 <- data_question2 %>% group_by(year) %>%
  mutate(mean_goal1_by_year=mean(goal1),
         mean_goal2_by_year=mean(goal2),
         mean_goal3_by_year=mean(goal3),
         mean_goal4_by_year=mean(goal4),
         mean_goal5_by_year=mean(goal5),
         mean_goal6_by_year=mean(goal6),
         mean_goal7_by_year=mean(goal7),
         mean_goal8_by_year=mean(goal8),
         mean_goal9_by_year=mean(goal9),
         mean_goal10_by_year=mean(goal10),
         mean_goal11_by_year=mean(goal11),
         mean_goal12_by_year=mean(goal12),
         mean_goal13_by_year=mean(goal13),
         mean_goal15_by_year=mean(goal15),
         mean_goal16_by_year=mean(goal16),
         mean_goal17_by_year=mean(goal17))
ggplot(data=data4) +
  geom_line(mapping=aes(x=year, y=mean_goal1_by_year,
                        mean_goal2_by_year,
                        mean_goal3_by_year,
                        mean_goal4_by_year,
                        mean_goal5_by_year,
                        mean_goal6_by_year,
                        mean_goal7_by_year,
                        mean_goal8_by_year,
                        mean_goal9_by_year,
                        mean_goal10_by_year,
                        mean_goal11_by_year,
                        mean_goal12_by_year,
                        mean_goal13_by_year,
                        mean_goal15_by_year,
                        mean_goal16_by_year,
                        mean_goal17_by_year))

# Evolution over time of the SDGs' achievement: mean by continent

# Evolution over time of the SDGs' achievement: mean by region

# Evolution over time of the SDGs' achievement: mean by country

### Third, compare the SDG achievement scores before and after 2015 (adoption at the UN)

# Mean comparison 2015-2016

# Mean comparison 2013-15 vs 2016-18 (let them some time to take measures)

