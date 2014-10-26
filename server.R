library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(glmnet)

# True classifier coefficients. Pick them randomlyl, from a random
# subset of the features
coefficients_real <- numeric(16)
reset_coefficients <- function() {
  nonzero_coefficients <- sample(1:16, 5, replace = FALSE)
  tmp <- numeric(16)
  tmp[nonzero_coefficients] <- runif(5, -1, 1)
  coefficients_real <<- tmp
}

# Evaluate a cutoff function based on a vector of coefficients
cutoff_func <- function(feature_matrix, coefficients) {
  as.vector(feature_matrix %*% coefficients)
}

# Generate class labels
class_labels <- function(feature_matrix, coefficients) {
  factor(ifelse(cutoff_func(feature_matrix, coefficients) > 0, "GOOD", "BAD"),
         levels = c("BAD", "GOOD"))
}

# Which are training, and which are test points
train_size  <- 50
test_size   <- 50
train_index <- 1:train_size
test_index  <- 1:test_size + train_size

# Transform the raw coordinates into all the possible features
transform_data <- function(data_set) {
    eval(feature_expressions, envir = data_set)
}

# A grid of points covering the domain, to help plot the dividing lines
grid <- data.frame(x = as.vector(matrix((-100:100)/100, 201, 201)),
                   y = as.vector(t(matrix((-100:100)/100, 201, 201))))
grid_transformed <- transform_data(grid)

# Generate points and labels.
all_points_raw         <- NULL
all_points_transformed <- NULL

# Plot function: plot a subset of the points, and a cutoff boundary.
# Also color the points.
plot_func <- function(subset) {
  ggplot(aes(x = x, y = y, col = outcome), data = grid) +
  geom_point(alpha = .05, size = 2) +
  geom_point(aes(shape = correct), data = all_points_raw[subset, ], size = 3) +
  xlab("") + ylab("")
}

# Render the current guess formula in LaTeX
make_formula <- function(coefficients) {
  # Intercept is a special case. Render it on the right.
  rhs <- format.default(-coefficients[1], digits = 2)
  lhs_coefs <- format.default(coefficients[2:16], digits = 2)
  lhs_terms <- feature_labels_raw[2:16]

  # If the coefficient is exactly 0 or -1, do not write explicitly.
  lhs_coefs[coefficients[2:16] == 1]  <- ""
  lhs_coefs[coefficients[2:16] == -1] <- "-"

  # Only include non-zero terms.
  present <- coefficients[2:16] != 0

  # Include a plus for non-negarive terms, except the first.
  pluses <- ifelse(coefficients[2:16] > 0, "+", "")
  first_plus <- match("+", pluses)
  if (!is.na(first_plus) && !any(coefficients[2:(first_plus + 1)] < 0)) {
    pluses[first_plus] <- ""
  }

  lhs <- paste(pluses[present], lhs_coefs[present], lhs_terms[present],
               sep = "", collapse = "")
  if (lhs == "") {
    lhs <- "0"
  }

  paste0("\\(", lhs, ">", rhs, "\\)")
}

# Flag to reset if we have shown the answer
seen_answer        <- FALSE
last_reset_pressed <- -1

# Cache the effective coefficients.
# Do not recalculate if they haven't changed
last_effective_coefficients <- numeric(16)

# ====================================================================
# Server
shinyServer(function(input, output, session) {
  # Initializations. Make sure we have generated the data before doing
  # anything else.
  started <- reactive({
    if (need_to_reset()) {
      # Rerun whenever "New Game" button is pressed
      input$restart

      # Make sure we get at least 20% of each class:
      repeat {
        reset_coefficients()
        all_points_raw <<- data.frame(x = runif(train_size + test_size, -1, 1),
                                      y = runif(train_size + test_size, -1, 1))
        all_points_transformed <<- transform_data(all_points_raw)
        all_points_raw$outcome <<- class_labels(all_points_transformed,
                                                coefficients_real)
        grid$outcome_real <<- class_labels(grid_transformed, coefficients_real)
        if (mean(all_points_raw$outcome == "GOOD") >= .2 &&
              mean(all_points_raw$outcome == "BAD")  >= .2) {
          break
        }
      }
    }

    TRUE
  })

  # Detect if 1. Just started, 2. Pressed New Game, or 3. Saw the answer.
  # If so, need to start over.
  need_to_reset <- reactive({
    if (input$tabs == "evaluate") {
      seen_answer <<- TRUE
      return(FALSE)
    }
    if (input$tabs == "build" && seen_answer) {
      seen_answer <<- FALSE
      last_reset_pressed <<- input$reset
      return(TRUE)
    }
    if (input$reset > last_reset_pressed) {
      # Special case, only run first time.
      if (input$reset == 0) {
        # Select default features. We need to initialize all of them, so
        # they get values initially, but we can change them after.
        updateCheckboxGroupInput(session, "features_selected",
                                 selected = c("1", "2", "5", "6"))
      }
      seen_answer <<- FALSE
      last_reset_pressed <<- input$reset
      return(TRUE)
    }
    FALSE
  })

  # Fit L1/L2 regressions for an auto-generated range of lambdas.
  regressions <- reactive({
    started()
    if (input$method == method_logistic_l2 ||
          input$method == method_logistic_l1) {
      if (input$method == method_logistic_l1) {
        alpha <- 1
      } else {
        alpha <- 0
      }
      index    <- features_present()

      # Treat the intercept on its own
      index[1] <- FALSE

      # Make sure we have at least two non-intercept variable:
      if (length(which(index)) < 2) {
        return(NULL)
      }
      fit <- glmnet(all_points_transformed[train_index, index, drop = FALSE],
                    all_points_raw$outcome[train_index] == "GOOD",
                    "binomial", alpha = alpha,
                    intercept = features_present()[1])
      fit
    }
  })

  # Remembar coefficient values, for when we disable/re-enable a feature
  last_coefficients_entered <- reactive({
    res <- c(input$coefficient_1, input$coefficient_2, input$coefficient_3,
             input$coefficient_4, input$coefficient_5, input$coefficient_6,
             input$coefficient_7, input$coefficient_8, input$coefficient_9,
             input$coefficient_10, input$coefficient_11, input$coefficient_12,
             input$coefficient_13, input$coefficient_14, input$coefficient_15,
             input$coefficient_16)
    if (length(res) < 16) {
      # First run. Not initialized yet. Return defaults
      res <- c(.5, 1, 0, 0, -1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    }
    res
  })

  last_lambda_selected <- reactive({
    started()
    if (input != method_manual) {
      input$lambda
    } else {
      NULL
    }
  })

  # Get the coefficient values
  coefficient_values <- reactive({
    started()
    index <- features_present()

    if (input$method == method_manual) {
      # Manual choice. Get them from the numericInputs
      coefs <- last_coefficients_entered()
    } else if (input$method == method_logistic_l2 ||
                 input$method == method_logistic_l1) {
      lambda <-input$lambda
      fit <- regressions()

      # If we had too few variables we didn't run the fit
      if (is.null(fit)) {
        return(numeric(16))
      }
      coefs <- numeric(16)
      # Which lambda value from the fit is closest to the chosen one?
      all_lambdas <- fit$lambda
      i <- which.min(abs(lambda - all_lambdas))
      sub_coefs <- as.vector(coef(fit)[, i])
      if (length(sub_coefs) == sum(index)) {
        coefs[index] <- sub_coefs
      }
    }

    coefs
  })

  # Which features are selected
  features_present <- reactive({
    started()
    res <- rep(FALSE, 16)
    ids <- input$features_selected
    res[as.integer(ids)] <- TRUE
    res
  })

  # Just the coefficients for the chosen features. Since they are changing
  # update all outputs
  effective_coefficients <- reactive({
    started()
    res <- coefficient_values() * features_present()
    if (any(res != last_effective_coefficients)) ({
      all_points_raw$guess   <<- class_labels(all_points_transformed, res)
      all_points_raw$correct <<- ifelse(all_points_raw$outcome ==
                                          all_points_raw$guess,
                                        "YES", "NO")
      grid$outcome           <<- class_labels(grid_transformed, res)
      last_effective_coefficients <<- res
    })
    res
  })

  # Only display results if there everything is done, and there
  # are no errors.
  data_ready <- reactive({
    any(effective_coefficients() != 0)
  })

  # Output variables
  output$plot_train <- renderPlot({
    if (data_ready()) {
      plot_func(train_index)
    }
  })
  output$plot_test  <- renderPlot({
    if (data_ready() && input$tabs == "evaluate") {
      plot_func(test_index)
    } else {
      "Calculating..."
    }
  })
  output$accuracy_train <- renderText({
    if (data_ready()) {
      percent(mean(all_points_raw$correct[train_index] == "YES"))
    } else {
      "Waiting for data..."
    }
    })
  output$accuracy_test <- renderText({
    if (data_ready() && input$tabs == "evaluate") {
      percent(mean(all_points_raw$correct[test_index] == "YES"))
    } else {
      "Calculating..."
    }
  })
  output$accuracy_total <- renderText({
    if (data_ready() && input$tabs == "evaluate") {
      percent(mean(grid$outcome == grid$outcome_real))
    } else {
      "Calculating..."
    }
  })
  output$formula_guess <- renderUI({
    if (data_ready()) {
      withMathJax(make_formula(effective_coefficients()))
    } else {
      "Calculating..."
    }
  })
  output$formula_real  <- renderUI({
    if (data_ready() && input$tabs == "evaluate") {
      withMathJax(make_formula(coefficients_real))
    } else {
      "Calculing..."
    }
  })

  # We can't call the same object twice?!?
  # So we just make an extra copy
  output$formula_guess2 <- renderUI({
    if (data_ready() && input$tabs == "evaluate") {
      withMathJax(make_formula(effective_coefficients()))
    } else {
      "Calculating..."
    }
  })

  # A small conditional numeric input
  # The main idea is from Stack-Overflow user Alex Brown
  # number = coefficient index
  conditionalInput <- function(number) {
    id <- paste0("coefficient_", number)
    enabled <- features_present()[number]
    if (enabled) {
      div(style="display:inline-block",
          withMathJax(tags$label(feature_labels[number], `for` = id)),
          tags$input(id = id, type = "number",
                     value = last_coefficients_entered()[number],
                     class = "input-mini"))
    }
  }
  output$model_input <- renderUI({
    withMathJax(
      if (input$method == method_manual) {
        tags$div(h4("Coefficients:"),
                 conditionalInput(1), conditionalInput(2),
                 conditionalInput(3), conditionalInput(4),
                 conditionalInput(5), conditionalInput(6),
                 conditionalInput(7), conditionalInput(8),
                 conditionalInput(9), conditionalInput(10),
                 conditionalInput(11), conditionalInput(12),
                 conditionalInput(13), conditionalInput(14),
                 conditionalInput(15), conditionalInput(16))
      } else {
        fit <- regressions()
        if (is.null(fit)) {
          # There were not enough features, so glmnet gave an error
          p("The model requires at least two non-intercept features.",
            "Please enable more features.")
        } else {
          lmin <- min(fit$lambda)
          lmax <- max(fit$lambda)
          llast <- last_lambda_selected()
          if (is.null(llast) || llast < lmin || llast > lmax) {
            llast <- .25 * lmax + .75* lmin
          }
        tags$div(if (input$method == method_logistic_l2) {
                   h4("\\(L_2\\) penalty term:")
                 } else {
                   h4("\\(L_1\\) penalty term:")
                 },
                 sliderInput("lambda", "\\(\\lambda\\)",
                             lmin, lmax, llast, ticks = FALSE))
        }
      })
  })

})

