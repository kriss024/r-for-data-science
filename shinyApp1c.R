# https://github.com/rstudio-education/shiny.rstudio.com-tutorial/tree/master/part-3-code

rm(list=ls())

library(shiny)

ui <- fluidPage(
  h1("My Shiny App"),
  p(style = "font-family:Impact",
    "See other apps in the",
    a("Shiny Showcase",
      href = "http://www.rstudio.com/
      products/shiny/shiny-user-showcase/")
  )
)

server <- function(input, output){}

shinyApp(ui, server)