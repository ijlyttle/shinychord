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

  # select item(s) to remove
  id_controller_item <- id_name("controller", "item")
  ui_controller$item <-
    shiny::uiOutput(id_controller_item)

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

    env$output[[id_controller_item]] <-
      shiny::renderUI({
        selectizeInput(
          inputId = id_controller_item,
          label = "Items",
          choices = rctval_names(rctval),
          selected = NULL,
          multiple = TRUE
        )
      })

    observeEvent(
      eventExpr = env$input[[id_controller_remove]],
      handlerExpr = {
        lapply(
          env$input[[id_controller_item]],
          function(x){
            rctval[[x]] <- NULL
          }
        )

      }
    )

    env$output[[id_view_status]] <-
      shiny::renderPrint({

        str_message_empty <- "List has no items"

        str_message_item <- "No item selected"

        # start by disabling all the controls
        shinyjs::disable(id_controller_item)
        shinyjs::disable(id_controller_remove)

        # validate that the list is not empty
        shiny::validate(
          shiny::need(length(rctval_names(rctval)) > 0, str_message_empty)
        )

        # passed the check, enable the selector
        shinyjs::enable(id_controller_item)

        # validate that we have a selection made
        shiny::validate(
          shiny::need(env$input[[id_controller_item]], str_message_item)
        )

        # enable the button
        shinyjs::enable(id_controller_remove)

        str_message <- paste0(
          "Items selected for removal: ",
          paste(env$input[[id_controller_item]], collapse = ", ")
        )

        cat(str_message)

      })

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )
}
