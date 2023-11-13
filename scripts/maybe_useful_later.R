library(sf)
library(leaflet)
library(shiny)
library(dplyr)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge data with the world map data
data0_sf <- merge(world, data_question2, by.x = "iso_a3", by.y = "code", all.x = TRUE)

# Transform the coordinate reference system to Robinson projection
data0_transformed <- st_transform(data0_sf, crs = "+proj=robin")

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Map with Overall Score"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:", min = 2000, max = 2022, value = 2022)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Filter data based on the selected year
  selected_data <- reactive({
    filter(data0_transformed, year == input$year)
  })
  
  # Create an interactive map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = selected_data(),
        color = "darkgrey",
        fill = TRUE,
        fillOpacity = 0.7,
        fillColor = ~colorNumeric("viridis")(selected_data()$overallscore, domain = NULL),
        stroke = TRUE
      ) %>%
      addLegend(
        "bottomright",
        title = "Overall Score",
        colors = colorNumeric("viridis")(selected_data()$overallscore, domain = NULL),
        values = ~overallscore
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)






























Freq_Missing_GDP <- ggplot(data = filtered_data_GDP) +
  geom_histogram(aes(x = GDPpercapita, 
                     fill = cut(PercentageMissing,
                                breaks = c(0, 0.25, 0.5, 1),
                                labels = c("0-24%", "25-49%", "50-100%"))),
                 bins = 100) +
  labs(title = "Histogram of GDP per capita", x = "GDP per capitaP", y = "Frequency") +
  scale_fill_manual(values = c("0-24%" = "blue", "25-49%" = "red", "50-100%" = "black"), labels = c("0-24%", "25-49%", "50-100%")) +
  guides(fill = guide_legend(title = "% missings")) +
  facet_wrap(~ code, nrow = 4)

print(Freq_Missing_GDP)

###### TEST #####

test <- data_question1 %>% filter(year>=2005)
mean(is.na(test))

see_missing_test <- test %>%
  group_by(code) %>%
  summarise(across(-c(goal1, goal10),  # Exclude columns "goal1" and "goal10"
                   ~ sum(is.na(.))) %>%
              mutate(num_missing = rowSums(across(everything()))) %>%
              filter(num_missing > 0))