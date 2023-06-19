#' Title Create a shiny app
#'
#' @param x Unused argument
#'
#' @return A shiny app
#' @export
#'
#' @examples
#' if(interactive()) {
#'   riskcalc_fun()
#' }
riskcalc_fun <-
  function(x)
  {
    # Make a UI
    ui <-
      shiny::fluidPage(
        title = "Example App",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::textInput(
              inputId = "test_input",
              label = "Add Text"
            )
          ),
          shiny::mainPanel(
            shiny::textOutput(outputId = "show_text")
          )
        )
      )

    # Make the server
    server <-
      function(input, output) {
        output$show_text <- shiny::renderText({input$test_input})
      }

    # Run the app
    shiny::shinyApp(ui, server)
  }
