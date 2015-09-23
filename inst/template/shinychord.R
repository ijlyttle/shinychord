#' Creates a collection of shiny objects to wrap the part of the dygraphs package.
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
#' The list \code{ui_controller} will have members:
#'
#' \describe{
#'  \item{\code{foo}}{bar}
#' }
#'
#' The list \code{ui_view} will have members:
#'
#' \describe{
#'  \item{\code{foo}}{bar}
#' }
#'
#' The function \code{server_model()} will be called from your server function.
#' Its arguments are:
#'
#' \describe{
#'  \item{\code{input, output, session}}{input, output, session values passed from your server function}
#' }
#'
#' @param id    character, tag to prepend to the input and output id's
#'
#' @return list containing \code{ui_controller}, \code{ui_view}, and \code{server_model}
#' @export
#'
template <- function(id){

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  name_out <- function(x){
    paste(x, ".out.", sep = "_")
  }

  ## ui_controller ##
  ui_controller <- shiny::tagList()


  ## ui_view ##
  ui_view <- shiny::tagList()

  # status
  id_view_status <- id_name("view", "status")
  ui_view$status <- shiny::verbatimTextOutput(id_view_status)

  ## server_model ##
  server_model <- function(input, output, session){

    # reactives

    # observers

    # outputs

    env$output[[id_view_status]] <- shiny::renderPrint({
      cat("hello world")
    })

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )

}
