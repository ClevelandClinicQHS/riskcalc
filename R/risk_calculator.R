#' Build a risk calculator
#'
#' @description Construct a \code{\link[shiny]{shiny}}-based risk calculator through different input options
#'
#' @param object An R object
#' @param ... Additional arguments
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
#'     inputs,
#'     outputs = outputs,
#'     title =
#'       paste(
#'         "Predicting 5-Year Recurrence-Free Survival",
#'         "after Radical Cystectomy for Bladder Cancer"
#'        ),
#'     intercept = -2.1264314
#'   )
#' }
risk_calculator <-
  function(object, ...) {
    UseMethod("risk_calculator", object)
  }

#' @rdname risk_calculator
#' @param outputs Outputs for risk calculator
#' @param title Title for risk calculator
#' @param citation Citation to display
#' @param app_name App shorthand (riskcalc.org/app_name)
#' @param intercept Intercept term to add to the linear combination
#' @export
risk_calculator.list <-
  function(
      object, # List of lists giving inputs
      outputs, # List giving outputs
      title = "", # Displays on top of app
      citation = "", # A citation to place in the app
      app_name = NULL, # A shorthand name for the app (riskcalc.org/app_name)
      intercept = 0, # Intercept to add to the linear predictor
      ...
    ) {

    inputs <- object

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
    ui <- get_UI(title, shiny_inputs, citation, app_name)

    # Make the server
    server <-
      function(input, output) {
        output$result <- DT::renderDataTable({eval(parse(text = final_table_expression))})
      }

    # Run the app
    shiny::shinyApp(ui, server)

  }

#' @rdname risk_calculator
#' @param result_label Calculation title in table
#' @param result_type See the \code{type} argument in \code{\link[stats]{predict.glm}}
#' @param result_format Function to format the calculated value
#' @export
risk_calculator.glm <-
  function(
    object, # A glm model object
    title = "",
    citation = "",
    app_name = NULL,
    result_label = "Risk",
    result_type = "response",
    result_format = function(x) paste0(round(100 * x, 1), "%"),
    ...
  ) {

    # Iterate to create the shiny input objects, server expressions, etc.
    inputs <- attr(object$terms, "dataClasses")[-1]
    shiny_inputs <- list()
    server_expressions <- c()
    for(i in seq_along(inputs)) {

      # Extract common arguments
      this_inputId <- names(inputs)[i]
      this_label <- names(inputs)[i]
      this_class <- inputs[i][[1]]

      # Check for numeric input
      if(this_class %in% c("numeric", "integer")) {

        # Create the input for numeric variable
        this_shiny_input <-
          shiny::textInput(
            inputId = this_inputId,
            label = this_label
          )

        # Set the server expression
        this_server_expression <- paste0(this_inputId, "=as.numeric(input$", this_inputId, ")")

        # Otherwise, default to select inputs
      } else {

        # Create the choice vector
        if(this_class == "logical") {

          # Set to logical values (as strings)
          these_choices <- c("FALSE", "TRUE")

          # Set the server expression
          this_server_expression <- paste0(this_inputId, "=as.logical(input$", this_inputId, ")")

        } else {

          # Extract levels from the model object
          these_choices <- object$xlevels[[this_inputId]]

          # Set the server expression
          this_server_expression <- paste0(this_inputId, "=input$", this_inputId)

        }

        # Create the input for categorical variable
        this_shiny_input <-
          shiny::selectInput(
            inputId = this_inputId,
            label = this_label,
            choices = these_choices
          )

      }

      # Add to the lists
      shiny_inputs[[i]] <- this_shiny_input
      server_expressions[i] <- this_server_expression

    }

    # Make the input data frame expression
    server_input_data <- paste0("data.frame(", paste(server_expressions, collapse = ","), ")")

    # Make a UI
    ui <- get_UI(title, shiny_inputs, citation, app_name)

    # Make the server
    server <-
      function(input, output) {

        # Make a reactive input data frame
        input_data <- shiny::reactive({eval(parse(text = server_input_data))})

        # Show result
        output$result <-
          DT::renderDataTable({
            data.frame(
              Result = result_label,
              Probability = result_format(stats::predict(object, newdata = input_data(), type = result_type))
            )
          })

      }

    # Run the app
    shiny::shinyApp(ui, server)

  }

# Internal function to create the user interface
get_UI <-
  function(title, shiny_inputs, citation, app_name) {

    # Make the page
    shiny::fluidPage(

      # Set the (default) theme
      theme = shinythemes::shinytheme("flatly"),

      # Define the function
      shiny::tags$script(js_email()),

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
          get_citation(citation),
          get_disclaimer(),
          get_links(app_name)
        )
      )
    )
  }

# Internal function to create disclaimer
get_disclaimer <-
  function() {
    shiny::wellPanel(
      htmltools::h3("Disclaimer"),
      htmltools::p("No Medical Advice. ALTHOUGH SOME CONTENT MAY BE PROVIDED BY INDIVIDUALS IN THE MEDICAL PROFESSION, YOU ACKNOWLEDGE THAT PROVISION OF SUCH CONTENT DOES NOT CREATE A MEDICAL PROFESSIONAL-PATIENT RELATIONSHIP AND DOES NOT CONSTITUTE AN OPINION, MEDICAL ADVICE, PROFESSIONAL DIAGNOSIS, SERVICE OR TREATMENT OF ANY CONDITION. Access to general information is provided for educational purposes only, through this site and links to other sites. Content is not recommended or endorsed by any doctor or healthcare provider. The information and Content provided are not substitutes for medical or professional care, and you should not use the information in place of a visit, call, consultation or the advice of your physician or other healthcare provider. You are liable or responsible for any advice, course of treatment, diagnosis or any other information, services or product obtained through this site.")
    )
  }

# Internal function to create citation
get_citation <-
  function(citation) {
    shiny::wellPanel(
      htmltools::h3("Click Below for Calculator and Author Contact Information"),
      htmltools::p(citation)
    )
  }

# Internal function to create JavaScript function for e-mail prompt
js_email <-
  function() {
    shiny::HTML(
      'function sendEmail() {
        var txt;
        if (confirm("This is a mailbox for reporting website errors to programmers for the risk calculator website.  If you have questions or concerns about a specific calculator, please use the calculator Author Contact Information found on the publication for that calculator.  Each calculator is documented by a specific publication with a corresponding author.")) {
          window.open("mailto:rcalcsupport@ccf.org");
        }
      }'
    )
  }

# Internal function to create links
get_links <-
  function(app_name) {

    # Check for an app name
    source_link <- "https://github.com/ClevelandClinicQHS/riskcalc-website"
    if(!is.null(app_name))
      source_link <- paste0(source_link, "/tree/main/", app_name)

    htmltools::p(
      htmltools::a("Homepage", href = "../", style = "font-family: 'Lato','Helvetica Neue',Helvetica,Arial,sans-serif; font-size: 15px;color: #2c3e50;font-weight: bold;text-align: center;text-decoration: underline;"),
      " | ",
      htmltools::a("Website Error Messages", href = "javascript:sendEmail()", style = "font-family: 'Lato','Helvetica Neue',Helvetica,Arial,sans-serif; font-size: 15px;color: #2c3e50;font-weight: bold;text-align: center;text-decoration: underline;"),
      " | ",
      htmltools::a("Add to phone (iOS Safari)", href="https://support.apple.com/guide/iphone/bookmark-favorite-webpages-iph42ab2f3a7/ios#iph4f9a47bbc", target="_blank", style = "font-family: 'Lato','Helvetica Neue',Helvetica,Arial,sans-serif; font-size: 15px;color: #2c3e50;font-weight: bold;text-align: center;text-decoration: underline;"),
      " | ",
      htmltools::a("Add to phone (Android)", href="https://www.cnet.com/tech/mobile/adding-one-touch-bookmarks-to-your-androids-home-screen/", target="_blank", style = "font-family: 'Lato','Helvetica Neue',Helvetica,Arial,sans-serif; font-size: 15px;color: #2c3e50;font-weight: bold;text-align: center;text-decoration: underline;"),
      " | ",
      htmltools::a("Source Code", href = source_link, style = "font-family: 'Lato','Helvetica Neue',Helvetica,Arial,sans-serif;font-size: 15px;color: #2c3e50;font-weight: bold;text-align: center;text-decoration: underline;"),

      style = "text-align: center;"
    )
  }
