library(shiny)
library(reshape)
library(ggplot2)

df <- as.data.frame(apply(mtcars, 2, mean))
colnames(df) <- c("mean")
df$cat <- rownames(df)
rownames(df) <- c(1:nrow(df))

plotList <- c()

getQuery <- function(session) {
  out <- tryCatch(
    {
      queryString <- (isolate(session$clientData$url_search))
      queryString <- gsub("[?]", "", queryString)
      queryString <- gsub("&", ",", queryString)
      eval(parse(text = paste("filters <- list(", queryString, ")", sep = "")))
      filters <- as.vector(unlist(filters))
      if (is.null(filters)) {
        filters <- repliace(11, TRUE)
      } else if (length(filters) != 11 | typeof(filters) != "logical") {
        filters <- repliace(11, TRUE)
      }
    },
    error = function(e) {
      return(replicate(11, TRUE))
    },
    warning = function(w) {
      return(replicate(11, TRUE))
    }
  )    
  return(out)
}

updateChecks <- function(session, filters) {
  updateCheckboxInput(session, "mpg", value = filters[1])
  updateCheckboxInput(session, "cyl", value = filters[2])
  updateCheckboxInput(session, "disp", value = filters[3])
  updateCheckboxInput(session, "hp", value = filters[4])
  updateCheckboxInput(session, "drat", value = filters[5])
  updateCheckboxInput(session, "wt", value = filters[6])
  updateCheckboxInput(session, "qsec", value = filters[7])
  updateCheckboxInput(session, "vs", value = filters[8])
  updateCheckboxInput(session, "am", value = filters[9])
  updateCheckboxInput(session, "gear", value = filters[10])
  updateCheckboxInput(session, "carb", value = filters[11])
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
      actionButton("update", "Update chart", icon("sync")),
      actionButton("url", "Get URL", icon("save"))
      ),
    mainPanel(id = "plots", 
              wellPanel(plotOutput("plot"))
              )
  )
)

server <- function(input, output, session){
  protocol <- (isolate(session$clientData$url_protocol))
  host <- (isolate(session$clientData$url_hostname))
  port <- (isolate(session$clientData$url_port))
  filters <- getQuery(session)
  updateChecks(session, filters)
  output$plot <- renderPlot({
    ggplot(df[filters, ], aes(x = cat, y = mean)) + geom_bar(stat = "identity", fill = "steelblue") + theme_minimal()
  })
  observeEvent(input$update, {
    filters <- c(input$mpg, input$cyl, input$disp, input$hp, input$drat, input$wt, input$qsec, input$vs, input$am, input$gear, input$carb)
    output$plot <- renderPlot({
      ggplot(df[filters, ], aes(x = cat, y = mean)) + geom_bar(stat = "identity", fill = "steelblue") + theme_minimal()
    })
  })
  observeEvent(input$url, {
    filters <- c(input$mpg, input$cyl, input$disp, input$hp, input$drat, input$wt, input$qsec, input$vs, input$am, input$gear, input$carb)
    url <- paste(protocol, host, ":", port, "/?",  sep = "")
    query <- paste("mpg=", toString(filters[1]),
                   "&cyl=", toString(filters[2]),
                   "&disp=", toString(filters[3]),
                   "&hp=", toString(filters[4]),
                   "&drat=", toString(filters[5]),
                   "&wt=", toString(filters[6]),
                   "&Wqsec=", toString(filters[7]),
                   "&vs=", toString(filters[8]),
                   "&am=", toString(filters[9]),
                   "&gear=", toString(filters[10]),
                   "&carb=", toString(filters[11]),
                   sep = "")
    showModal(
      modalDialog(title = "URL",
                  p(paste(url, query, sep = ""),
                    style = "word-break: break-all !important"),
                  easyClose = TRUE)
    )
  })
}

shinyApp(ui = ui, server = server)
