#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Advanced Prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("text", "Enter a value:", "50"),
      radioButtons("operation", "Choose an operation:",
                   choices = list("Multiply by 2" = "multiply", "Square the value" = "square")),
      actionButton("predict", "Predict"),
      br(),
      p("Enter a numeric value in the text box, choose an operation, and click 'Predict' to see the result."),
      p("This application can either multiply the entered value by 2 or square the value based on your choice.")
    ),
    
    mainPanel(
      h3("Prediction Output:"),
      verbatimTextOutput("prediction")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$predict, {
    output$prediction <- renderText({
      input_value <- as.numeric(input$text)
      operation <- input$operation
      prediction <- if (operation == "multiply") {
        input_value * 2
      } else {
        input_value^2
      }
      paste("Predicted value:", prediction)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
