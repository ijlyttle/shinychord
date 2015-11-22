#' Creates a collection of shiny objects to wrap a compound time-input.
#'
#' The list will contain:
#'
#' \describe{
#'   \item{\code{ui_controller}}{\code{shiny::taglist} of ui elements for the controller}
#'   \item{\code{ui_view}}{\code{shing::taglist} of ui elements for the view}
#'   \item{\code{server_model}}{function with reactive code}
#' }
#'
#' The list returned by this function has to be available to both the ui and the server.
#' If not using the \code{shinyApp} formulation, perhaps \code{global.R} could be useful.
#'
#' The list \code{ui_controller} will have inputs:
#'
#' \describe{
#'  \item{\code{hours}}{\code{shiny::numericInput} used to set the number of hours}
#'  \item{\code{minutes}}{\code{shiny::numericInput} used to set the number of minutes}
#' }
#'
#' The list \code{ui_view} will have members:
#'
#' \describe{
#'  \item{\code{view_text}}{\code{shiny::textOutput} displays vector of hours and minutes}
#' }
#'
#' The function \code{server_model()} will be called from your server function.
#' Its arguments are:
#'
#'
#' \describe{
#'  \item{\code{input, output, session}}{input, output, session values passed from your server function}
#'  \item{\code{rctval_input, item_input}}{
#'    \code{shiny::reactiveValues} object, character string.
#'    \code{rctval_input[[item_input]]} lubridate duration, output - number of seconds
#'  }
#' }
#'
#' @param id      character, tag to prepend to the input and output id's
#' @param label   character, label for the compound input
#' @param step    numeric vector for the steps, (hours, minutes)
#' @param default numeric lubridate duration for the default
#'
#' @return list containing \code{ui_controller}, \code{ui_view}, and \code{server_model}
#' @export
#
ch_input_time_compound <- function(id, label = "", step = c(1, 5), default = c(1, 30)) {

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  id_controller_hour <- id_name("controller", "hour")
  id_controller_minute <- id_name("controller", "minute")

  input_hour <-
    shiny::tagAppendAttributes(
      shiny::numericInput(
        inputId = id_controller_hour,
        label = NULL,
        value = default[[1]],
        min = 0,
        max = NA,
        step = step[[1]],
        width = "75px"
      ),
      class = "shinychord-input-time-compound"
    )

  input_minute <-
    shiny::tagAppendAttributes(
      shiny::numericInput(
        inputId = id_controller_minute,
        label = NULL,
        value = default[[2]],
        min = 0,
        max = 59,
        step = step[[2]],
        width = "75px"
      ),
      class = "shinychord-input-time-compound"
    )

  ## ui_controller ##
  ui_controller <- shiny::tagList(
    shiny::tags$label(label, `for` = id_controller_hour),
    shiny::div(
      input_hour,
      tags$span("hr."),
      input_minute,
      tags$span("min.")
    )
  )

  ## ui_view ##
  ui_view <- shiny::tagList()

  # text view of time
  id_text_time <- id_name("view", "time")
  ui_view$status <- shiny::verbatimTextOutput(id_text_time)

  ## server_model ##
  server_model <- function(
    input, output, session,
    rctval_input, item_input
  ){

    shiny::observe({
      rctval_input[[item_input]] <-
        lubridate::dhours(input[[id_controller_hour]]) +
        lubridate::dminutes(input[[id_controller_minute]])
    })

    ## outputs ##
    output[[id_text_time]] <-
      shiny::renderText({
        format(rctval_input[[item_input]])
      })

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )
}
