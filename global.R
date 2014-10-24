# Possible feature to include in the model.

# HTML labels to display. (Use LaTeX via MathJax)
feature_labels <-
  c("\\(1\\)",   "\\(y\\)",     "\\(y^2\\)",     "\\(y^3\\)",
    "\\(x\\)",   "\\(x y\\)",   "\\(x y^2\\)",   "\\(x y^3\\)",
    "\\(x^2\\)", "\\(x^2 y\\)", "\\(x^2 y^2\\)", "\\(x^2 y^3\\)",
    "\\(x^3\\)", "\\(x^3 y\\)", "\\(x^3 y^2\\)", "\\(x^3 y^3\\)")

# Character IDs for use between the UI and the server
feature_ids <- as.character(1:16)

# Use the labels as names for checkboxGroupInput
names(feature_ids) <- feature_labels

feature_expressions <-
  c(quote(1),   quote(y),       quote(y^2),       quote(y^3),
    quote(x),   quote(x * y),   quote(x * y^2),   quote(x * y^3),
    quote(x^2), quote(x^2 * y), quote(x^2 * y^2), quote(x^2 * y^3),
    quote(x^3), quote(x^3 * y), quote(x^3 * y^3), quote(x^3 * y^3))
