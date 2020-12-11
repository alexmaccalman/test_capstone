#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Predict the Next Word"),
    
    textInput("phrase", "Input Text to Predict", width = 1000),
    h3(helpText("Listed here are the top 5 predicted words.")),
    tableOutput("predicts")
)

# Define server logic 
server <- function(input, output) {
  
    output$predicts <- renderTable({
        phrase_input <- input$phrase
        table <- predict_word(phrase_input)
        table
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
