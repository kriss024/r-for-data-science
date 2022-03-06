rm(list=ls())

library(shiny)

ui <- fluidPage(
  "Hello, world! HTML"
)

server <- function(input, output) {
}

shinyApp(ui, server)