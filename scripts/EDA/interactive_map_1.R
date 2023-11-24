library(here)
library(sf)
library(rnaturalearth)
library(shiny)
library(plotly)
library(shinytest)

# Import data
data_question2 <- read.csv(here("scripts", "data", "data_question24.csv"), sep=",")

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge data with the world map data
data0 <- merge(world, data_question2, by.x = "iso_a3", by.y = "code", all.x = TRUE)

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

# Set up slider steps
slider_steps <- lapply(unique(data0$year), function(year) {
  selected_data <- data0 %>% filter(year == year) # Subset data for the selected year
  list(
    args = list(
      "z", list(selected_data$overallscore),
      "locations", list(selected_data$iso_a3),
      "text", list(~paste("Country: ", selected_data$name, "<br>SDG score: ", selected_data$overallscore)),
      "visible", list(data0$year == year)
    ),
    label = as.character(year),
    method = "restyle"
  )
})

# Set up slider
graphx <- plot_ly(
  layout(
    sliders = list(
      list(
        active = 2022,
        steps = slider_steps
      )
    )
  ),
  type = "choropleth",
  colors = c("darkred", "orange", "yellow", "darkgreen"),
  colorbar = list(title = "SDG score"),
  hoverinfo = "text"
)
# Display the plot
graphx
