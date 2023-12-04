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

# Test 1

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
    filter(data0, year == as.character(input$year))
  })
  
  # Create a plotly object with hover information
  output$map <- renderPlotly({
    plot_ly(
      type = "choropleth",
      z = ~selected_data()$overallscore,
      locations = ~selected_data()$iso_a3,
      text = ~paste("Country: ", selected_data()$name, "<br>Overall Score: ", selected_data()$overallscore),
      colors = c("darkred", "orange", "yellow", "darkgreen"),
      colorbar = list(title = "Overall Score", cmin = 40, cmax = 87),
      zmin = 40,
      zmax = 87,
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
mapApp <- shinyApp(ui, server)
mapApp
shinylive::export("mapApp", "report")

# Test 2
# Sample data loading (replace with your actual data loading)
data_question2 <- read.csv(here("scripts", "data", "data_question24.csv"), sep=",")

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge data with the world map data
data0 <- merge(world, data_question2, by.x = "iso_a3", by.y = "code", all.x = TRUE)

data0 <- data0 %>%
  filter(!is.na(overallscore))

unique_years <- unique(data0$year)

plot_ly(
  type = "choropleth",
  z = ~data0$overallscore[data0$year==2000],
  locations = ~data0$iso_a3[data0$year==2000],
  text = ~paste("Country: ", data0$name[data0$year==2000], "<br>Overall Score: ", data0$overallscore[data0$year==2000]),
  colors = c("darkred", "orange", "yellow", "darkgreen"),
  colorbar = list(title = "Overall Score", cmin = 40, cmax = 87),
  zmin = 40,
  zmax = 87,
  hoverinfo = "text"
) %>%
  layout(
    title = "SDG overall score evolution",
    sliders = list(
      list(
        active = 0,
        currentvalue = list(prefix = "Year: "),
        steps = lapply(seq_along(unique_years), function(i) {
          year <- unique_years[i]
          print(paste("Year:", year, "Number of Observations:", sum(data0$year == year)))
          list(
            label = as.character(year),
            method = "restyle",
            args = list(
              list(z = list(data0$overallscore[data0$year == year]),
                   text = ~paste("Country: ", data0$name[data0$year==year], "<br>Overall Score: ", data0$overallscore[data0$year==year]))
            )
          )
        })
      )
    )
  )
