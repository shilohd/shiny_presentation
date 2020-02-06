library(shiny)
library(DT)
library(ggplot2)

stringToLink <- function(row) {
  id <- gsub(" ", "", row["model"])
  link <- actionLink(id, row["model"], onclick = paste("Shiny.setInputValue('link', '", row["model"], "', {priority: 'event'})", sep = ""))
  return(toString(link))
}

ui <- fluidPage(
  DT::dataTableOutput('carTable')
)

server <- function(input, output, session){
  df <- mtcars
  df$model <- rownames(df)
  rownames(df) <- c(1:length(rownames(df)))
  df <- df[, c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]
  df$model <- apply(df, 1, stringToLink)
  output$carTable <- DT::renderDataTable(df, escape = FALSE)
  observeEvent(input$link, {
    showModal(modalDialog(
      tags$img(src = paste(input$link, ".jpg", sep = ""), width = "100%"),
      easyClose = TRUE
    ))
    }
  )
}

shinyApp(ui = ui, server = server)
