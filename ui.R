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
      checkboxGroupInput(inputId  = "features_selected_1",
                         label    = "Include features:",
                         choices  = feature_ids[1:4],
                         inline   = TRUE),
      checkboxGroupInput(inputId  = "features_selected_2",
                         choices  = feature_ids[5:8],
                         label    = "",
                         inline   = TRUE),
      checkboxGroupInput(inputId  = "features_selected_3",
                         choices  = feature_ids[9:12],
                         label    = "",
                         inline   = TRUE),
      checkboxGroupInput(inputId  = "features_selected_4",
                         choices  = feature_ids[13:16],
                         label    = "",
                         inline   = TRUE),
      hr()
    ),

    mainPanel(
      # Plot the test points
      plotOutput("plot")
    )
  )
))
