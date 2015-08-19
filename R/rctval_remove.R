#' rctval_remove
#'
#' Creates a collection of shiny objects to manage the removal of members
#' of a set of reactive values.
#'
#' The list will contain:
#'
#' \itemize{
#'   \item \code{ui_controller} \code{shiny::taglist} of ui elements for the controller
#'   \item \code{ui_view} \code{shing::taglist} of ui elements for the view
#'   \item \code{server_model} a function with reactive code - this function takes a \code{reactiveValues} as its arguement
#' }
#'
#' The list returned by the factory has to be available to both the ui and the server. If not using the \code{shinyApp}
#' formulation, perhaps \code{global.R} could be useful.
#'
#' The component \code{server_model} will be called in the server function, using the
#' particular \code{reactiveValue} you wish to associate with the "thing".
#'
#' @param id    character, tag to prepend to the input and output id's
#'
#' @return list containing \code{ui_controller}, \code{ui_view}, and \code{srv_model}
#' @export
#'
rctval_remove <- function(id) {

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  ## ui_controller ##
  ui_controller <- shiny::tagList()

  # select element(s) to remove
  id_controller_elem <- id_name("controller", "elem")
  ui_controller$elem <-
    shiny::uiOutput(id_controller_elem)

  # button to remove element
  id_controller_remove <- id_name("controller", "remove")
  ui_controller$remove <-
    actionButton(
      inputId = id_controller_remove,
      label = "Remove",
      class = "btn-danger"
    )

  ## ui_view ##
  ui_view <- shiny::tagList()

  # status of the possibility to add a member
  id_view_status <- id_name("view", "status")
  ui_view$status <- verbatimTextOutput(id_view_status)

  ## server_model ##
  server_model <- function(rctval){

    env = parent.frame()

    env$output[[id_controller_elem]] <-
      shiny::renderUI({
        selectizeInput(
          inputId = id_controller_elem,
          label = "Items",
          choices = rctval_names(rctval),
          selected = NULL,
          multiple = TRUE
        )
      })

    observeEvent(
      eventExpr = env$input[[id_controller_remove]],
      handlerExpr = {
        rctval[[env$input[[id_controller_elem]]]] <- NULL
      }
    )

    env$output[[id_view_status]] <-
      shiny::renderPrint({

        str_message_empty <- "List is empty"

        str_message_elem <- "No elements selected"

        shinyjs::disable(id_controller_remove)

        shiny::validate(
          shiny::need(length(names(rctval)) > 0, str_message_empty)
        )

        shiny::validate(
          shiny::need(env$input[[id_controller_elem]], str_message_elem)
        )

        shinyjs::enable(id_controller_remove)

        cat(env$input[[id_controller_elem]])
      })

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )
}
