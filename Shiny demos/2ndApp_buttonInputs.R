#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load the required library
library(shiny)

# Define the UI for application that draws a dynamic line of text that updates with user 
# input. This is how we can dynamically change graph titles for example.
ui <- fluidPage(
  h2(radioButtons("choices", "Display of the data", c("OSD", "CPD"))),
  h3(textOutput("data_displayed"))
)

# Define server logic required to draw user-input text
server <- function(input, output) {
  output$data_displayed <-  renderText({
    paste0("You have chosen to display the ", input$choices, " data")
  })
}

# Run the application 
shinyApp(ui, server)
