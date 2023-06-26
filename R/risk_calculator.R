#' Build a risk calculator
#'
#' @description Construct a shiny-based risk calculator given information about the inputs and outputs
#'
#' @param inputs Inputs for risk calculator
#' @param outputs Outputs for risk calculator
#' @param title Title for risk calculator
#' @param intercept Intercept term to add to the linear combination
#'
#' @return A shiny app
#' @export
#'
#' @examples
#' # Example from https://riskcalc.org/bladderCancer/
#' # Source code: https://github.com/ClevelandClinicQHS/riskcalc-website/tree/main/bladderCancer
#'
#' # Build set of inputs
#' inputs <-
#'
#'   # Each element is one input (risk factor) in that order
#'   list(
#'     Gender =
#'       list(
#'         label = "Gender",
#'         levels = c("Female", "Male"), # Indicates categorical
#'         weights = c(0, 0.034072352)
#'       ),
#'     AgeYr =
#'       list(
#'         label = "Age (Years)",
#'         range = c(20, 100), # Indicates numeric
#'         x_trans =
#'           list(
#'             \(x)x,
#'             \(x)max(x - 44, 0)**3,
#'             \(x)max(x - 62, 0)**3,
#'             \(x)max(x - 77, 0)**3
#'            ), # Each of these contributes to weighted sum
#'         weights = c(0.0041362795, 7.4691198e-06, 1.6432063e-05, 8.9629437e-06)
#'       ),
#'     Stage97 =
#'       list(
#'         label = "RC Tumor Pathology",
#'         levels = c("pT0", "pTa", "pTis", "pT1", "pT2", "pT3", "pT4"),
#'         weights = c(0, 0.87060903, 0.55456587, 1.0188265, 1.2332424, 1.8741757, 2.4566641)
#'       ),
#'     Histology =
#'       list(
#'         label = "RC Tumor Histology",
#'         levels = c("ADENOCARCINOMA", "SCC", "TCC"),
#'         weights = c(0, 0.34940226, 0.72706351)
#'       ),
#'     Grade =
#'       list(
#'         label = "RC Tumor Grade",
#'         levels = c("GX", "High", "Low"),
#'         weights = c(0, 0.17941245, 0.69780658)
#'       ),
#'     NodeResult =
#'       list(
#'         label = "Lymph Node Status",
#'         levels = c("NX", "Negative", "Positive"),
#'         weights = c(0, 0.29095514, 0.87448703)
#'       ),
#'     DxToRC =
#'       list(
#'         label = "Days Between Dx and RC (Days)",
#'         range = c(0, 100000),
#'         x_trans =
#'           list(
#'             \(x)x,
#'             \(x)max(x - 0.45996875, 0)**3,
#'             \(x)max(x - 3.5483304, 0)**3,
#'             \(x)max(x - 27.808397, 0)**3
#'            ),
#'         weights = c(0, 2.9994932e-05, 3.3813355e-05, 3.8184231e-06)
#'       )
#'   )
#'
#' # Build outputs to display in the table
#' outputs <-
#'   list(
#'     res =
#'       list(
#'         label = "Percentage of 5-Year Recurrence-Free Survival",
#'         lp_trans = \(x) 100 * 0.7524126 ** exp(x) # Transformation for linear predictor
#'       )
#'   )
#'
#' # Create the risk calculator
#' if(interactive()) {
#'   risk_calculator(
#'     inputs = inputs,
#'     outputs = outputs,
#'     title =
#'       paste(
#'         "Predicting 5-Year Recurrence-Free Survival",
#'         "after Radical Cystectomy for Bladder Cancer"
#'        ),
#'     intercept = -2.1264314
#'   )
#' }
#'
risk_calculator <-
  function(
      inputs, # List of lists giving inputs
      outputs, # List giving outputs
      title = "", # Displays on top of app
      intercept = 0 # Intercept to add to the linear predictor
    ) {

    # Iterate to create the shiny input objects, server expressions, etc.
    shiny_inputs <- list()
    server_expressions <- c()
    for(i in seq_along(inputs)) {

      # Extract common arguments
      this_inputId <- names(inputs)[i]
      this_input <- inputs[[i]]
      this_label <- this_input$label
      this_server_expression <- c()

      # Check for numeric input
      if("range" %in% names(this_input)) {

        # Create the input for numeric variable
        this_shiny_input <-
          shiny::textInput(
            inputId = this_inputId,
            label = this_label,
            placeholder = paste(this_input$range, collapse = "-")
          )

        # Iterate the transformation functions to build the expressions
        for(j in seq_along(this_input$weights))
          this_server_expression[j] <- paste0(this_input$weights[j], "*inputs[['", this_inputId, "']]$x_trans[[", j, "]](as.numeric(input$", this_inputId, "))")

      # Other default to categorical
      } else {

        # Create the input for categorical variable
        this_shiny_input <-
          shiny::selectInput(
            inputId = this_inputId,
            label = this_label,
            choices = this_input$levels
          )

        # Iterate the transformation functions to build the expressions
        for(j in seq_along(this_input$weights))
          this_server_expression[j] <- paste0(this_input$weights[j], "*(input$", this_inputId, "=='", this_input$levels[j], "')")

      }

      # Make the final server expression
      this_server_expression <- paste(this_server_expression, collapse = "+")

      # Add to lists
      shiny_inputs[[i]] <- this_shiny_input
      server_expressions[i] <- this_server_expression
    }

    # Make the final server expression for the linear predictor
    lp_server_expression <- paste(intercept, paste(server_expressions, collapse = "+"), sep = "+")

    # Make the expressions for the rows of the output table
    result_row_names <- c()
    result_row_values <- c()
    for(i in seq_along(outputs)) {

      # Extract the current output (row)
      this_output <- outputs[[i]]
      this_output_id <- names(outputs)[i]

      # Add to vectors
      result_row_names[i] <- paste0("outputs$", this_output_id, "$label[", i, "]")
      result_row_values[i] <- paste0("outputs$", this_output_id, "$lp_trans(", lp_server_expression, ")")

    }

    # Make final table expression
    final_table_expression <- paste0("data.frame(Result=c(", paste(result_row_names, collapse = ","), "), Probability=c(", paste(result_row_values, collapse = "+"), "))")

    # Make a UI
    ui <-

      # Make the page
      shiny::fluidPage(

        # Set the (default) theme
        theme = shinythemes::shinytheme("flatly"),

        # Make the title panel
        shiny::titlePanel(title),

        # Make the default layout
        shiny::sidebarLayout(

          # Create the side bar with input objects
          do.call(
            shiny::sidebarPanel,
            shiny_inputs
          ),

          # Create the main display panel
          shiny::mainPanel(

            # Add button for calculation
            shiny::actionButton(inputId = "run_calculator", "Run Calculator"),
            htmltools::br(),
            htmltools::hr(),

            # Make the table output
            DT::dataTableOutput(outputId = "result"),
            htmltools::br(),

            # Information panels
            shiny::wellPanel(htmltools::h3("Click Below For More Info"), htmltools::p("Citation")),
            shiny::wellPanel(htmltools::h3("Disclaimer"), htmltools::p("No Advice."))
          )
        )
      )

    # Make the server
    server <-
      function(input, output) {
        output$result <- DT::renderDataTable({eval(parse(text = final_table_expression))})
      }

    # Run the app
    shiny::shinyApp(ui, server)

  }
