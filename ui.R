library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Classifier Game"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # Load MathJax
      withMathJax(),

      # Render checkboxes in a 4 x 4 grid
      h3("Model choice"),
      tags$div(h4("Features to include:"),
               checkboxGroupInput("features_selected", "", feature_ids,
                         selected = c(feature_ids), inline = TRUE)),
      tags$div(h4("Method choice:"),
               selectInput("method", "How do we pick the coefficients?",
                           method_choices, method_manual)),
      tags$div(uiOutput("model_input"))
    ),

    mainPanel(
      # Plot the test points
      plotOutput("plot"),
      tags$p("Current guess for classifier: ",
             htmlOutput("formula", inline = TRUE)),
      tags$p("In-sample accurcy: ",
             textOutput("accuracy", inline = TRUE))
    )
  )
))
