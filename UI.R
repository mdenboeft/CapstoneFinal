library(shiny)

# Define UI for application that predicts next word
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Final Data Science Capstone Ptoject: The Next Word Prediction"),
  fluidRow(HTML("By Madelon den Boeft") ),
  fluidRow(HTML("<i> 20 January 2019</i>") ),
  
  
  fluidRow(
    br(),
    p("When a user provides a sentence without the last word, this prediction model predicts the next word of that sentence.")), 
  br(),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("InputString", "Here you can enter a sentence without the last word",value = ""),
      actionButton("do", "Predict the next word")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4("The next word is"),
      fluidRow(column(5, verbatimTextOutput("PredictedWord", placeholder = TRUE)))
    )
  )
))


