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
interactive_map_1 <- shinyApp(ui, server)
interactive_map_1

# Save as html document
app_dir <- shinyAppDir(
  system.file("appdir", package="shinytest"), 
  appFiles = c("ui.R", "server.R")
)

save_html(interactive_map_1, file = here("scripts", "EDA", "interactive_map_1.html"))
