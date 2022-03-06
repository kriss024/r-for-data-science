# https://github.com/rstudio-education/shiny.rstudio.com-tutorial

rm(list=ls())

library(shiny)

ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  
  textInput("name", "What's your name?"),
  
  textOutput("greeting")
)

server <- function(input, output) {
  
  output$greeting <- renderText({
    ifelse(input$name == '', '', paste0("Hello ", input$name, " !"))
  })
  
}

shinyApp(ui, server)