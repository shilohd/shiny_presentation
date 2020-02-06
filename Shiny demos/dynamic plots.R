library(shiny)
library(reshape)
library(ggplot2)

df <- as.data.frame(apply(mtcars, 2, mean))
colnames(df) <- c("mean")
df$cat <- rownames(df)
rownames(df) <- c(1:nrow(df))

plotList <- c()

# Add a new chart if the same chart does not already exist
addChart <- function(data, filters, input, output) {
  id <- gsub(", ", "", toString(as.numeric(filters)))
  if (!id %in% plotList) {
    insertUI("#plots", where = 'beforeEnd', 
             wellPanel(id = paste("panel", id, sep = ""),
                       actionButton("close", icon("times"), onClick = paste("Shiny.setInputValue('closeWell', '", id, "', {priority: 'event'})", sep = "")),
                       plotOutput(paste("plot", id, sep = "")))
             )
     output[[paste("plot", id, sep = "")]] <- renderPlot({
       ggplot(data, aes(x = cat, y = mean)) + geom_bar(stat = "identity", fill = "steelblue") + theme_minimal()
     })
     plotList <<- append(plotList, id)
   }
}

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
      actionButton("add", "Add", icon("chart-bar"))
    ),
    mainPanel(id = "plots", wellPanel(plotOutput("plot")))
  )
)

server <- function(input, output, session){
  output$plot <- renderPlot({
    ggplot(df, aes(x = cat, y = mean)) + geom_bar(stat = "identity", fill = "steelblue") + theme_minimal()
  })
  observeEvent(input$add, {
    filters <- c(input$mpg, input$cyl, input$disp, input$hp, input$drat, input$wt, input$qsec, input$vs, input$am, input$gear, input$carb)
    addChart(df[filters, ], filters, input, output)
  })
  observeEvent(input$closeWell, {
    plotList <<- plotList[!plotList == input$closeWell]
    removeUI(paste("#panel", input$closeWell, sep = ""))
  })
}

shinyApp(ui = ui, server = server)
