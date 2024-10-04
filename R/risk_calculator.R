#' Build a risk calculator
#'
#' @description Create a (templated) risk calculator for www.riskcalc.org, optionally populating using an existing model object
#'
#' @param ... Generic arguments
#'
#' @return A \code{\link[shiny]{shiny}} application
#' @export
risk_calculator <-
  function(...) {
    UseMethod("risk_calculator")
  }

#' @rdname risk_calculator
#' @param app_name Application name (e.g., \code{"AppExample"} for www.riskcalc.org/AppExample. If no name is provided, a generic name "riskcalc_app" is used.
#' @param app_directory Location to export the application. If none provided, defaults to the working directory.
#' @export
risk_calculator.default <-
  function(
    app_name = NULL,
    app_directory = NULL,
    ...
    ) {

    # Export the app template
    export_app(app_directory, app_name, "", "", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)

  }

#' @rdname risk_calculator
#' @param model A \code{\link[stats]{glm}} or \code{\link[survival]{coxph}} object
#' @param title Title for risk calculator
#' @param citation Citation(s) and author information
#' @param label Label for the calculated value
#' @param label_header Header label for the result descriptor
#' @param value_header Header label for the result value
#' @param format Function to format the predicted value for display
#' @param labels Named character vector specifying labels for any inputs
#' @param levels Named list of named character vectors specifying labels for any factor levels
#' @param placeholders Named character vector specifying range limits for numeric input variables (of the form \code{c(var="<min>-<max>")}). Invokes calls to \code{\link[shiny]{validate}} enforcing the limits.
#' @export
risk_calculator.glm <-
  function(
    model,
    app_name = NULL,
    app_directory = NULL,
    title = "",
    citation = "",
    label = "Predicted Value",
    label_header = "Result",
    value_header = "Value",
    format = NULL,
    labels = NULL,
    levels = NULL,
    placeholders = NULL,
    ...
  ) {

    # Set the formatting function to display as-is by default
    if(is.null(format))
      format <- function(x) x

    # Set the prediction engine
    prediction_engine <- readLines(system.file("prediction_engines", "prediction_engine_glm.R", package = "riskcalc"))

    # Export the app with model inputs
    export_app(app_directory, app_name, title, citation, build_io_expressions(model, labels, levels, placeholders), model, label, label_header, value_header, format, NULL, prediction_engine, NULL)

  }

#' @rdname risk_calculator
#' @param time Single time point to calculate survival probability (see \code{\link[survival]{summary.survfit}})
#' @export
#' @examples
#' library(survival)
#'
#' # Make a data set
#' dat <- gbsg
#' dat$meno <- factor(dat$meno)
#'
#' # Build a model
#' mod <-
#'   coxph(
#'     formula = Surv(rfstime, status) ~ age + meno,
#'     data = dat
#'   )
#'
#' # Time point of interest
#' time <- median(dat$rfstime) # 1084
#'
#' # Create the risk calculator
#' \dontrun{
#' risk_calculator(
#'   model = mod,
#'   time = time,
#'   app_name = "BreastCancer",
#'   app_directory = getwd(),
#'   title = "Risk of Death or Recurrence in Breast Cancer",
#'   citation =
#'     paste0(
#'      "Patrick Royston and Douglas Altman, External validation of a Cox prognostic",
#'      "model: principles and methods. BMC Medical Research Methodology 2013, 13:33"
#'     ),
#'   label_header = "`Probability Of Death or Recurrence`",
#'   label = paste0("At time = ", time),
#'   labels = c(age = "Age (years)", meno = "Menopausal Status"),
#'   levels = list(meno = c(`0` = "Premenopausal", `1` = "Postmenopausal")),
#'   placeholders = c(age = "21-80"),
#'   format = function(x) paste0(round(100 * (1 - x), 2), "%")
#' )
#' }
risk_calculator.coxph <-
  function(
    model,
    time,
    app_name = NULL,
    app_directory = NULL,
    title = "",
    citation = "",
    label = "Survival Probability",
    label_header = "Result",
    value_header = "Value",
    format = NULL,
    labels = NULL,
    levels = NULL,
    placeholders = NULL,
    ...
  ) {

    # Check time input
    if(missing(time))
      stop("Please specify a survival time point of interest.")

    # Check for type
    if(!(class(time) %in% c("numeric", "integer")))
      stop("Survival time point must be numeric")

    # Check for length
    if(length(time) > 1)
      stop("Please select a single time point of interest.")

    # Set the formatting function to display as-is by default
    if(is.null(format))
      format <- function(x) paste0(round(100 * x, 2), "%")

    # Set the prediction engine
    prediction_engine <- readLines(system.file("prediction_engines", "prediction_engine_coxph.R", package = "riskcalc"))

    # Export the app with model inputs
    export_app(app_directory, app_name, title, citation, build_io_expressions(model, labels, levels, placeholders), model, label, label_header, value_header, format, "survival", prediction_engine, time)

  }

# Internal function to build the various ui/server expressions
build_io_expressions <-
  function(model, labels, levels, placeholders) {

    # Extract the model terms
    inputs <- attr(model$terms, "dataClasses")[-1]

    # Check if any terms are not in the model
    if(!all(names(inputs) %in% attr(model$terms, "term.labels")))
      stop("Term(s) not included in model found in formula. Please only specify model via inclusion.")

    # Specify the ids, types, and labels
    input_ids <- names(inputs)
    input_types <- unname(inputs)
    input_labels <- input_ids

    # If specified, setup the clean labels
    if(!is.null(labels))
      for(i in seq_along(labels))
        input_labels[which(input_labels == names(labels)[i])] <- labels[i]

    # Create placeholders for object storage
    server_expressions <- c()
    server_validations <- c()
    shiny_input_export_strings <- c()

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
          server_validations <- c(server_validations, paste0("validate(need(!is.na(as.numeric(input$", this_inputId, ")) & as.numeric(input$", this_inputId, ") >= ", these_limits[1], " & as.numeric(input$", this_inputId, ") <= ", these_limits[2], ", 'Input a valid ", this_label, "'))"))

        }

        # Create the export string vector
        this_shiny_input_export_string <-
          c(
            paste0("# ", this_label),
            "textInput(",
            paste0('  inputId = "', this_inputId, '",'),
            paste0('  label = "', this_label, '",'),
            paste0('  placeholder = "', this_placeholder, '"'),
            "),",
            ""
          )

        # Set the server expression
        this_server_expression <- paste0(this_inputId, " = as.numeric(input$", this_inputId, ")")

      # Otherwise, default to select inputs
      } else {

        # Create the choice vector
        if(this_class == "logical") {

          # Set to logical values (as strings)
          these_choices <- c("FALSE", "TRUE")

          # Set the server expression
          this_server_expression <- paste0(this_inputId, " = as.logical(input$", this_inputId, ")")

        } else {

          # Extract levels from the model object
          these_choices <- model$xlevels[[this_inputId]]

          # Set the server expression
          this_server_expression <- paste0(this_inputId, " = input$", this_inputId)

        }

        # Set the default names of the choices
        names(these_choices) <- these_choices

        # Check if the current input has new labels to be assigned
        if(this_inputId %in% names(levels))
          for(j in seq_along(levels[[this_inputId]]))
            names(these_choices)[which(these_choices == names(levels[[this_inputId]])[j])] <- unname(levels[[this_inputId]][j])

        # Create the export string vector
        this_shiny_input_export_string <-
          c(
            paste0("# ", this_label),
            "selectInput(",
            paste0('  inputId = "', this_inputId, '",'),
            paste0('  label = "', this_label, '",'),
            paste0('  choices = c(', paste(paste0(names(these_choices), ' = "', these_choices, '"'), collapse = ","), ')'),
            "),",
            ""
          )

      }

      # Add to the lists
      server_expressions[i] <- this_server_expression
      shiny_input_export_strings <- c(shiny_input_export_strings, this_shiny_input_export_string)

    }

    # Make the input data frame expression
    server_input_data <- paste0("data.frame(", paste(server_expressions, collapse = ", "), ")")

    # Return a list of objects
    list(
      server_input_data  = server_input_data,
      server_validations = server_validations,
      shiny_input_export_strings = shiny_input_export_strings
    )

  }

# Internal function to create the app directory and return the path
create_app_directory <-
  function(app_directory, app_name) {

    # Set the directory name
    if(is.null(app_name)) {

      # Set default name
      app_name <- "riskcalc_app"

      # Give warning
      warning("No 'app_name' provided. Defaulting to 'riskcalc_app'.")

    }

    # Set the directory location
    if(is.null(app_directory))
      app_directory <- getwd()

    # Set the new app directory
    new_directory <- paste0(app_directory, "/", app_name)

    # Create the directory if it doesn't exist yet
    if(file.exists(new_directory)) {

      # Print message that it was created
      message(paste0("App directory already exists: '", new_directory, "'. Will overwrite files."))

    } else {

      # Create the directory
      dir.create(new_directory)

      # Print message that it was created
      message(paste0("App directory created: '", new_directory, "'"))

    }

    # Return the path for subsequent use
    new_directory

  }

# Internal function to build the UI export file
export_ui <-
  function(new_directory, title, citation, app_name, shiny_input_export_strings) {

    ## Retrieve UI template as a character string for each line
    temp_ui <- readLines(system.file("app_template", "ui.R", package = "riskcalc"))

    ## Substitute the title
    temp_ui[26] <- paste0('    titlePanel("', title, '"),')

    ## Substitute the citation

    # Format based on input
    if(is.null(citation)) {
      citation <- "p()"
    } else if("html" %in% class(citation) | "shiny.tag" %in% class(citation)) {
      citation <- paste0("HTML(\"", as.character(citation), "\")")
    } else if(is.character(citation)) {
      citation <- paste0('p("', citation, '")')
    } else {
      stop("Citation should be a character string, an HTML() object, or a shiny tag.")
    }

    # Add to line
    temp_ui[51] <- paste0('          ', citation)

    ## Substitute the links
    if(!is.null(app_name))
      app_name <- paste0("tree/main/", app_name)
    temp_ui[70] <- paste0("          a(\"Source Code\", href = \"https://github.com/ClevelandClinicQHS/riskcalc-website/", app_name, "\", style = \"font-family: 'Lato','Helvetica Neue',Helvetica,Arial,sans-serif;font-size: 15px;color: #2c3e50;font-weight: bold;text-align: center;text-decoration: underline;\"),")

    ## Append the inputs
    if(!is.null(shiny_input_export_strings)) {

      # Small edit at the end of the inputs
      shiny_input_export_strings[length(shiny_input_export_strings) - 1] <- ")"

      # Create the vector of lines to add to the file
      shiny_input_lines <-
        c(
          "      sidebarPanel(",
          "",
          paste0("        ", shiny_input_export_strings),
          "      ),"
        )

      # Create the new vector
      temp_ui <-
        c(
          temp_ui[1:31],
          shiny_input_lines,
          temp_ui[34:length(temp_ui)]
        )

    }

    # Write to file in app directory
    writeLines(temp_ui, con = paste0(new_directory, "/ui.R"))

  }

# Internal function to export the server
export_server <-
  function(new_directory, server_input_data, server_validations, label, label_header, value_header) {

    ## Retrieve server template as a character string for each line
    temp_server <- readLines(system.file("app_template", "server.R", package = "riskcalc"))

    ## Server expressions
    temp_server_expressions <- NULL
    index_offset <- 0

    # Substitute the validation requirements
    if(!is.null(server_validations)) {

      temp_server_expressions <-
        c(
          "",
          "# Input validation",
          server_validations
        )
    }

    # Substitute input expression
    if(!is.null(server_input_data)) {

      # Create expression
      temp_server_expressions <-
        c(
          temp_server_expressions,
          "",
          "# Create input data frame",
          server_input_data
        )

      # Update server vector
      temp_server <-
        c(
          temp_server[1:10],
          paste0("          ", temp_server_expressions),
          temp_server[17:length(temp_server)]
        )

      # Update the offset
      index_offset <- index_offset + length(temp_server_expressions)

      # Add the result display
      temp_server[20 + length(temp_server_expressions)] <- paste0("          data.frame(", label_header, " = \"", label, "\", ", value_header, " = format(prediction_engine(input_data())))")

    }

    # Write to file in app directory
    writeLines(temp_server, con = paste0(new_directory, "/server.R"))

  }

# Internal function to export the global config file
export_global <-
  function(new_directory, model, format, packages, prediction_engine, time) {

    ## Retrieve global template as a character string for each line
    temp_global <- readLines(system.file("app_template", "global.R", package = "riskcalc"))

    # Add package dependencies
    if(!is.null(packages)) {

      # Add the import calls for each imported package
      temp_global <-
        c(
          temp_global[1:7],
          paste0("library(", packages, ")"),
          temp_global[8:length(temp_global)]
        )

    }

    # Model I/O
    if(!is.null(model)) {

      # Export to app directory
      save(model, file = paste0(new_directory, "/model.RData"))

      # Add import statement
      temp_global <-
        c(
          temp_global,
          "",
          "# Load the app model",
          "load(\"model.RData\")"
        )

      # Add the timepoint if needed
      if(!is.null(time))
        temp_global <- c(temp_global, "", "# Time horizon", paste0("time <- ", time))

    }

    # Append with the prediction engine function to be used for computing output
    if(!is.null(prediction_engine))
      temp_global <- c(temp_global, "", "# Function to get prediction", prediction_engine, "", "# Formatting function")

    # Write to file in app directory
    writeLines(temp_global, con = paste0(new_directory, "/global.R"))

    # Append with the formatting function to be used to display output
    if(!is.null(model))
      dump("format", file = paste0(new_directory, "/global.R"), append = TRUE)

  }

# Internal function to export the application
export_app <-
  function(
    app_directory,
    app_name,
    title,
    citation,
    io_expressions,
    model,
    label,
    label_header,
    value_header,
    format,
    packages,
    prediction_engine,
    time
  ) {

    # Extract the I/O expressions
    server_input_data <- io_expressions$server_input_data
    server_validations <- io_expressions$server_validations
    shiny_input_export_strings <- io_expressions$shiny_input_export_strings

    # Create the new directory
    new_directory <- create_app_directory(app_directory, app_name)

    # Export the UI
    export_ui(new_directory, title, citation, app_name, shiny_input_export_strings)

    # Export server
    export_server(new_directory, server_input_data, server_validations, label, label_header, value_header)

    # Export global
    export_global(new_directory, model, format, packages, prediction_engine, time)

    #utils::file.edit(paste0(new_directory, "/ui.R")) Try to open files when created

    # Exit silently
    invisible()

  }
