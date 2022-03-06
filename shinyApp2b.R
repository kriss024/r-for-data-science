# https://github.com/rstudio-education/shiny.rstudio.com-tutorial/tree/master/part-3-code

rm(list=ls())

library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarLayout(
  sidebarPanel(
    
  titlePanel("Hello Shiny!"),  
  
  sliderInput(inputId = "num", 
              label = "Choose a offset", 
              value = 25, min = 1, max = 100),
  
  ),
  
  plotOutput("hist")
)
)

server <- function(input, output) {
  
  output$hist <- renderPlot({
    
    n<-1000
    offset<-input$num/5
    mean1 = 50 - offset
    mean2 = 50 + offset
    sd = 10
    
    df <- data.frame(
      sex=factor(rep(c("F", "M"), each=n)),
      weight=round(c(rnorm(n, mean=mean1, sd=sd), rnorm(n, mean=mean2, sd=sd)))
    )
    
    ggplot(df, aes(x=weight, color=sex)) +
      geom_density(alpha=.5, fill="white") +
      scale_color_brewer(palette="Accent") +
      theme_minimal() +
      theme(legend.position="top")
  })
  
}

shinyApp(ui, server)