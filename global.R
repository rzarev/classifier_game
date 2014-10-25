# Possible feature to include in the model.

library(shiny)

# HTML labels to display. (Use LaTeX via MathJax)
feature_labels_raw <-
  c("1",   "y",     "y^2",     "y^3",
    "x",   "x y",   "x y^2",   "x y^3",
    "x^2", "x^2 y", "x^2 y^2", "x^2 y^3",
    "x^3", "x^3 y", "x^3 y^2", "x^3 y^3")
feature_labels <- paste("\\(", feature_labels_raw, "\\)", sep = "")
# Special case the intercept:
feature_labels[1] <- "Intercept"

# Character IDs for use between the UI and the server
feature_ids <- as.character(1:16)

# Use the labels as names for checkboxGroupInput
names(feature_ids) <- feature_labels

# We can evaluate this in environment where x and y are vectors.
feature_expressions <-
  quote(cbind(1, matrix(c(y, y^2, y^3, x, x * y, x * y^2, x * y^3, x^2,
                          x^2 * y, x^2 * y^2, x^2 * y^3, x^3, x^3 * y,
                          x^3 * y^3, x^3 * y^3), , 15)))

# Choices for models:
method_manual      <- "Pick them by hand"
method_logistic_l1 <- "Logistic regression with L1 pealty"
method_logistic_l2 <- "Logistic regression with L2 pealty"
method_choices <- c(method_manual,
                    method_logistic_l1,
                    method_logistic_l2)

