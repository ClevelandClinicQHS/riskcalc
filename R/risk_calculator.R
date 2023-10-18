#' Build a risk calculator
#'
#' @description Construct a formatted \code{\link[shiny]{shiny}}-based risk calculator through different model inputs
#'
#' @param model A \code{\link[stats]{glm}} object
#' @param ... Additional arguments
#'
#' @return A \code{\link[shiny]{shiny}} application
#' @export
#'
#' @examples
#' if(interactive()) {
#'   risk_calculator(glm(1~1))
#' }
risk_calculator <-
  function(model, ...) {
    UseMethod("risk_calculator", model)
  }

#' @rdname risk_calculator
#' @param title Title for risk calculator (see \code{\link[shiny]{titlePanel}})
#' @param citation Citation(s) and author information
#' @param label Label for the calculated value
#' @param label_header Header label for the result descriptor
#' @param value_header Header label for the result value
#' @param format Function to format the predicted value for display
#' @param app_name App shorthand, like \code{"AppExample"} (https://riskcalc.org/AppExample/)
#' @param labels Named character vector specifying labels for any inputs
#' @param levels Named list of named character vectors specifying labels for any factor levels
#' @param placeholders Named character vector specifying range limits for numeric input variables (of the form \code{c(var="<min>-<max>")}). Invokes calls to \code{\link[shiny]{validate}} enforcing the limits.
#' @export
risk_calculator.glm <-
  function(
    model,
    title = "",
    citation = "",
    label = "Predicted Value",
    label_header = "Result",
    value_header = "Value",
    format = NULL,
    app_name = NULL,
    labels = NULL,
    levels = NULL,
    placeholders = NULL,
    ...
  ) {

    ## Setup the input parameters

    # Extract the model inputs
    inputs <- attr(model$terms, "dataClasses")[-1]

    # Specify the ids, types, and labels
    input_ids <- names(inputs)
    input_types <- unname(inputs)
    input_labels <- input_ids

    # If specified, setup the clean labels
    if(!is.null(labels))
      for(i in seq_along(labels))
        input_labels[which(input_labels == names(labels)[i])] <- labels[i]

    # Set the formatting function to display as-is by default
    if(is.null(format))
      format <- function(x) x

    # Create placeholders for object storage
    shiny_inputs <- list()
    server_expressions <- c()
    server_validations <- c()

    # Iterate to create the shiny input objects, server expressions, etc.
    for(i in seq_along(inputs)) {

      # Extract common arguments
      this_inputId <- input_ids[i]
      this_class <- input_types[i]
      this_label <- input_labels[i]

      # Check for numeric input
      if(this_class %in% c("numeric", "integer")) {

        # Set this input's placeholder
        this_placeholder <- NULL
        if(this_inputId %in% names(placeholders)) {

          # Set the placeholder to display
          this_placeholder <- placeholders[which(names(placeholders) == this_inputId)]

          # Parse the limits
          these_limits <- as.numeric(strsplit(this_placeholder, "\\s{0,1}-\\s{0,1}")[[1]])

          # Add validation expression
          server_validations <- c(server_validations, paste0("shiny::validate(shiny::need(!is.na(as.numeric(input$", this_inputId, "))&as.numeric(input$", this_inputId, ")>=", these_limits[1], "&as.numeric(input$", this_inputId, ")<=", these_limits[2], ",'Input a valid ", this_label, "'))"))

        }

        # Create the input for numeric variable
        this_shiny_input <-
          shiny::textInput(
            inputId = this_inputId,
            label = this_label,
            placeholder = this_placeholder
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
          these_choices <- model$xlevels[[this_inputId]]

          # Set the server expression
          this_server_expression <- paste0(this_inputId, "=input$", this_inputId)

        }

        # Set the default names of the choices
        names(these_choices) <- these_choices

        # Check if the current input has new labels to be assigned
        if(this_inputId %in% names(levels))
          for(j in seq_along(levels[[this_inputId]]))
            names(these_choices)[which(these_choices == names(levels[[this_inputId]])[j])] <- unname(levels[[this_inputId]][j])

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

    # Make the server validation expression for numeric inputs
    server_validations <- paste(server_validations, collapse = ";")

    # Make a UI
    ui <- get_UI(title, shiny_inputs, citation, app_name)

    # Make the server
    server <- get_server(server_input_data, format, label, model, server_validations, label_header, value_header)

    # Run the app
    shiny::shinyApp(ui, server)

  }

# Internal function to create the server function
get_server <-
  function(server_input_data, format, label, model, server_validations, label_header, value_header) {

    function(input, output) {

      # Make a reactive input data frame
      input_data <-
        shiny::eventReactive(
          input$run_calculator, {

            # Check for validations
            if(server_validations != "")
              eval(parse(text = server_validations))

            # Build the input data set
            eval(parse(text = server_input_data))

          })

      # Show result
      output$result <-
        DT::renderDataTable(
          {
            # Make the data frame
            result_dat <-
              data.frame(
                Result = label,
                Value = format(stats::predict(model, newdata = input_data(), type = "response"))
              )

            # Set the headers
            colnames(result_dat) <- c(label_header, value_header)
            result_dat
          },
          options =
            list(
              pageLength = 10,
              lengthMenu = 0,
              searching = 0,
              info = 0,
              paging = 0,
              initComplete =
                DT::JS(
                  "function(settings, json) {
                    $(this.api().table().header()).css({'background-color': '#606060', 'color': '#fff'});
                  }"
                )
            ),
          rownames = FALSE
        )

    }

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
