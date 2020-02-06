library(shiny)
library(reshape)
library(ggplot2)

df <- as.data.frame(apply(mtcars, 2, mean))
colnames(df) <- c("mean")
df$cat <- rownames(df)
rownames(df) <- c(1:nrow(df))

plotList <- c()

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxInput("mpg", "mpg", TRUE),
      checkboxInput("cyl", "cyl", TRUE),
      checkboxInput("disp", "disp", TRUE),
      checkboxInput("hp", "hp", TRUE),
      checkboxInput("drat", "drat", TRUE),
      checkboxInput("wt", "wt", TRUE),
      checkboxInput("qsec", "qsec", TRUE),
      checkboxInput("vs", "vs", TRUE),
      checkboxInput("am", "am", TRUE),
      checkboxInput("gear", "gear", TRUE),
      checkboxInput("carb", "carb", TRUE),
      actionButton("update", "Update chart", icon("sync")),
      actionButton("url", "Get URL", icon("save"))
      ),
    mainPanel(id = "plots", 
              wellPanel(plotOutput("plot"))
              )
  )
)

server <- function(input, output, session){
  output$plot <- renderPlot({
    ggplot(df, aes(x = cat, y = mean)) + geom_bar(stat = "identity", fill = "steelblue") + theme_minimal()
  })
  observeEvent(input$update, {
    filters <- c(input$mpg, input$cyl, input$disp, input$hp, input$drat, input$wt, input$qsec, input$vs, input$am, input$gear, input$carb)
    output$plot <- renderPlot({
      ggplot(df[filters, ], aes(x = cat, y = mean)) + geom_bar(stat = "identity", fill = "steelblue") + theme_minimal()
    })
  })
  observeEvent(input$url, {
    
  })
}

shinyApp(ui = ui, server = server)
