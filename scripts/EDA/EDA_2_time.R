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

# Save as html document

### First, look at the evolution of SDG achievement overall score over time

# Evolution over time of the overall score: mean over all countries

data1 <- data_question2 %>% group_by(year) %>%
  mutate(mean_overall_score_by_year=mean(overallscore))

ggplot(data=data1) +
  geom_line(mapping=aes(x=year, y=mean_overall_score_by_year), color="blue", lwd=1) +
  scale_y_continuous(limits = c(0, 100))

# Evolution over time of the overall score: mean by continent

data2 <- data_question2 %>% group_by(year, continent) %>%
  mutate(mean_overall_score_by_year=mean(overallscore))

ggplot(data=data2) +
  geom_line(mapping=aes(x=year, y=mean_overall_score_by_year, color=continent), lwd=1)+
  scale_y_continuous(limits = c(0, 100))

# Evolution over time of the overall score: mean by region

data3 <- data_question2 %>% group_by(year, region) %>%
  mutate(mean_overall_score_by_year=mean(overallscore))

ggplot(data=data3) +
  geom_line(mapping=aes(x=year, y=mean_overall_score_by_year, color=region), lwd=1)+
  scale_y_continuous(limits = c(0, 100))

### Second, look at the evolution of SDG achievement scores (16) over time

# Evolution over time of the SDGs' achievement: mean over all countries

data4 <- data_question2 %>%
  group_by(year) %>%
  summarise(across(starts_with("goal"), mean, na.rm=TRUE)) %>%
  pivot_longer(cols = starts_with("goal"), names_to = "goal", values_to = "mean_value")

color_palette <- c("red", "blue", "green", "orange", "purple", "pink", "brown", "gray", "cyan", "magenta", "yellow", "darkgreen", "darkblue", "darkred", "darkorange", "darkcyan")

ggplot(data = data4) +
  geom_line(mapping = aes(x = year, y = mean_value, color = goal), size = 0.7) +
  scale_color_manual(values = color_palette) +
  scale_y_continuous(limits = c(0, 100)) +
  guides(
    color = guide_legend(
      ncol = 2,        # Number of columns
      title.position = "top",  # Position of the legend title
      title.hjust = 0.5        # Horizontal justification of the legend title
    )
  )

ggplot(data = data4) +
  geom_line(mapping = aes(x = year, y = mean_value), size = 0.7) +
  scale_color_manual(values = color_palette) +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ goal, nrow = 4)

# Evolution over time of the SDGs' achievement: mean by continent
data5 <- data_question2 %>%
  group_by(year, continent) %>%
  summarise(across(starts_with("goal"), mean, na.rm=TRUE)) %>%
  pivot_longer(cols = starts_with("goal"), names_to = "goal", values_to = "mean_value")

color_palette <- c("red", "blue", "green", "orange", "purple", "pink", "brown", "gray", "cyan", "magenta", "yellow", "darkgreen", "darkblue", "darkred", "darkorange", "darkcyan")

ggplot(data = data5) +
  geom_line(mapping = aes(x = year, y = mean_value, color=continent), size = 0.7) +
  scale_color_manual(values = color_palette) +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ goal, nrow = 4)

# Evolution over time of the SDGs' achievement: mean by region
data6 <- data_question2 %>%
  group_by(year, region) %>%
  summarise(across(starts_with("goal"), mean, na.rm=TRUE)) %>%
  pivot_longer(cols = starts_with("goal"), names_to = "goal", values_to = "mean_value")

color_palette <- c("red", "blue", "green", "orange", "purple", "pink", "brown", "gray", "cyan", "magenta", "yellow", "darkgreen", "darkblue", "darkred", "darkorange", "darkcyan")

ggplot(data = data5) +
  geom_line(mapping = aes(x = year, y = mean_value, color=region), size = 0.7) +
  scale_color_manual(values = color_palette) +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ goal, nrow = 4)

# Evolution over time of the SDGs' achievement: mean by country

### Third, compare the SDG achievement scores before and after 2015 (adoption at the UN)

# Mean comparison 2015-2016

# Mean comparison 2013-15 vs 2016-18 (let them some time to take measures)

