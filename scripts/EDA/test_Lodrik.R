library(ggplot2)
library(shiny)

data <- read.csv(here("scripts", "data", "data_question1.csv"))


ui <- fluidPage(
  titlePanel("Interactive Violin Plot"),
  selectInput("continent", "Choose a Continent:", 
              choices = unique(data$continent)),
  plotOutput("violinPlot")
)

server <- function(input, output) {
  output$violinPlot <- renderPlot({
    # Filter data based on selected continent
    filtered_data <- data[data$continent == input$continent,]
    # Create the violin plot
    ggplot(filtered_data, aes(x = variable, y = value)) + 
      geom_violin() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

AM <- read.csv(here("scripts", "data", "All_Merge.csv"))
