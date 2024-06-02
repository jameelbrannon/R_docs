library(shiny)

shinyUI(fluidPage(
  titlePanel("Next Word Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("phrase", "Enter a phrase:", value = ""),
      actionButton("predict", "Predict Next Word")
    ),
    
    mainPanel(
      h3("Predicted Next Word:"),
      verbatimTextOutput("prediction")
    )
  )
))