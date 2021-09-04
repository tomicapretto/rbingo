#' Run application
#'
#' @return An object that represents the app. Printing the object or passing it
#' to runApp() will run the app.
#'
#' @importFrom shiny observeEvent eventReactive reactiveValues invalidateLater
#' @importFrom shiny observe req isolate isTruthy
#' @importFrom shiny renderUI uiOutput moduleServer stopApp htmlOutput HTML
#' @importFrom shiny numericInput updateNumericInput
#' @importFrom shiny selectInput updateSelectInput
#' @importFrom shiny dateInput updateDateInput
#' @importFrom shiny textInput
#' @importFrom shiny renderText verbatimTextOutput
#' @importFrom shiny actionButton actionLink downloadButton downloadHandler
#' @importFrom shiny withProgress incProgress
#' @importFrom shiny wellPanel tabPanel tagList tags column fluidRow NS
#' @importFrom utils tail

#' @export
run_app <- function(options = list(launch.browser=TRUE)) {
  shiny::shinyApp(app_ui(), app_server, options = options)
}
