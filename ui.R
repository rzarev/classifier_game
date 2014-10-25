library(shiny)

buildPanel <-
  tabPanel("Build a model",
           sidebarLayout(
             sidebarPanel(
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
               h4("Training sample", align = "center"),
               plotOutput("plot_train"),
               tags$p("Current guess for the classifier: ",
                      htmlOutput("formula_guess", inline = TRUE)),
               tags$p("In-sample accurcy: ",
                      textOutput("accuracy_train", inline = TRUE))
             )
           )
  )

evaluatePanel <-
  tabPanel("Evaluete your model",
           tags$div(
              h4("Validation sample", align = "center"),
              plotOutput("plot_test")
              ,
              tags$p("Final guess for the classifier: ",
                     htmlOutput("formula_guess2", inline = TRUE))
              ,
              tags$p("New-sample accurcy: ",
                     textOutput("accuracy_test", inline = TRUE))
              ,
              tags$p("Real classifier: ",
                  htmlOutput("formula_real", inline = TRUE))
              ,
              tags$p("Exact out-of-sample accurcy: ",
                  textOutput("accuracy_total", inline = TRUE))
           ))

helpPanel <-
  tabPanel("Help",
           withMathJax(),
           h4("Description"),
           p("This application aims to explore the difficulties of fitting",
             "a model with a small sample size without overfitting."),
           p("The goal of the game is to guess the distribution the sample",
             "points come from. The possible models you can choose from",
             "are all given by a cutoff function, which is linear in",
             "powers of the \\(x\\) and \\(y\\) coordinates."),
           p("You can individually enable or disable each possible monomial.",
             "If you allow all monomial, then the true function is guaranteed",
             "to be within the allowed set. However, by including more",
             "terms, you increase the degrees of freedom. Since the sample",
             "size is small (50 points), this increases the risk of",
             "overfitting and performing poorly on an independent",
             "validation sample."),
           h4("Options"),
           p("There are three possible modes of model selection:"),
           tags$ol(
             tags$li("Manual mode. In this mode you pick the coefficients directly."),
             tags$li("Logistic regression with \\(L_1\\) penalty. This is also known",
                "as the \"Lasso\" penalty. We model the points as drawn from a",
                "Bernoulli",
                "distribution, where the log-odds are linear function of the",
                "feature variables. Complex models are penalized, so that",
                "the smallest coefficients are sent to 0. The parameter",
                "\\(\\lambda\\), which you get to control, decides how much",
                "to penalize. Setting it to 0 means no penalty, while",
                "setting to \\(\\infty\\) sets all coefficients to 0."),
             tags$li("Logistic regression with \\(L_2\\) penalty. This is also",
                "known as \"Ridge\" penalty. The only difference from",
                "the previous option is the type of penalty.",
                "In this case, all coefficients are shrunk by the same",
                "proprotion.")
             ),
           p("In all cases, on the right side you can see your current",
             "guess, and how it performs on the sample."),
           h4("Evaluation"),
           p("When you have settled on a model, you can click on the",
             "evaluation tab, where you will see how your model",
             "performs on a new, held out sample."),
#            p("[To simulate a real-world scenario, you can only use the",
#              "validation set once. When you go to the model-building",
#              "tab again, a new simulation will start with a different",
#              "classifier."),
           h4("Theory"),
           p("In theory, the more complex the model you try, the bigger",
             "the disconnect between the in-sample and out-of-sample",
             "performance. In this case adding more features increases",
             "the complexity of the model, while regularization",
             "techniques, like \\(L_1\\) or \\(L_2\\) penalties",
             "attempt to bring the complexity down, while still",
             "keeping the model as flexible as possible."),
           p("To read more about this, you can start at the",
             "Wikipedia page on ",
             tags$a(href = "http://en.wikipedia.org/wiki/Regularization_%28mathematics%29",
                    "regularization"),"."))

navbarPage("Classifier Game",
          tabsetPanel(buildPanel, evaluatePanel, helpPanel))
