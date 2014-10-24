library(shiny)
library(dplyr)
library(ggplot2)

# A fixed classifier funciton as a stub only
cutoff <- function(x, y) {3 *x^2 *y ^3 - x + y^2}

# Generate class labels based on the cutoff
classifier <- function(x, y) {
  ifelse(cutoff(x, y) > 0, "GOOD", "BAD")
}

# Which are training, and which are test points
train_index = 1:50
test_index  = 51:100

# Use data_frame from dplyr, instead of data.frame, so we can
# use earlier column in the definition of later ones.
all_points <- data_frame(x       = runif(50, -1, 1),
                         y       = runif(50, -1, 1),
                         outcome = classifier(x, y),
                         stringsAsFactors = TRUE)

shinyServer(function(input, output) {
    # Create a plot of the training data
    output$plot <- renderPlot({
      ggplot(aes(x = x, y = y, col = outcome),
             data = all_points[train_index]) +
      geom_point()
    })
  })

