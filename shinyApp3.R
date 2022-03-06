# https://github.com/rstudio-education/shiny.rstudio.com-tutorial

rm(list=ls())

library(shiny)
library(ggplot2)

ui <- fluidPage(
  headerPanel("Hello Shiny!"),
  
  sidebarPanel(
    
    sliderInput(inputId = "sample", 
                label = "Choose a sample", 
                value = 500, min = 100, max = 1000),
    
    sliderInput(inputId = "num", 
              label = "Choose a offset", 
              value = 25, min = 1, max = 100),
    
    sliderInput(inputId = "sd", 
                label = "Choose a s.d.", 
                value = 10, min = 1, max = 20),
    
    textInput(inputId = "title", 
              label = "Write a title",
              value = "Histogram of Random Normal Values")
  ),

  mainPanel(
    plotOutput("hist")
  )
)

server <- function(input, output) {
  
  output$hist <- renderPlot({
    
    n<-input$sample
    offset<-input$num/5
    mean1 = 50 - offset
    mean2 = 50 + offset
    sd = input$sd
    
    df <- data.frame(
      sex=factor(rep(c("F", "M"), each=n)),
      weight=round(c(rnorm(n, mean=mean1, sd=sd), rnorm(n, mean=mean2, sd=sd)))
    )
    
    ggplot(df, aes(x=weight, color=sex)) +
      geom_density(alpha=.5, fill="white") +
      scale_color_brewer(palette="Accent") +
      theme_minimal() +
      labs(title=input$title) +
      theme(legend.position="top")
  })
  
}

shinyApp(ui, server)