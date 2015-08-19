#' rctval_add
#'
#' Creates a collection of shiny objects to manage the addition of component
#' of one set reactive values to another.
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
rctval_add <- function(id) {

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  ## ui_controller ##
  ui_controller <- shiny::tagList()

  # name of the new element
  id_controller_name <- id_name("controller", "name")
  ui_controller$name <-
    shiny::textInput(
      inputId = id_controller_name,
      label = "Name",
      value = ""
    )

  # button to add element
  id_controller_add <- id_name("controller", "add")
  ui_controller$add <-
    shiny::actionButton(
      inputId = id_controller_add,
      label = "Add",
      class = "btn-primary"
    )

  ## ui_view ##
  ui_view <- shiny::tagList()

  # status of the possibility to add a member
  id_view_status <- id_name("view", "status")
  ui_view$status <- shiny::verbatimTextOutput(id_view_status)

  # status of the list, in general
  id_view_members <- id_name("view", "members")
  ui_view$members <- shiny::verbatimTextOutput(id_view_members)

  ## server_model ##
  server_model <- function(rctval_source, comp_source, rctval_dest){

    env = parent.frame()

    env$output[[id_view_status]] <-
      shiny::renderPrint({

        is_name_valid <- function(x){
          x == make.names(x)
        }

        str_message_name <-
          paste(
            "Name must:",
            "- begin with non-numeric character",
            "- not contain spaces",
            "- not contain some special characters",
            sep = "\n"
          )

        str_message_source <-
          "Source item is empty."

        shinyjs::disable(id_controller_add)

        shiny::validate(
          shiny::need(is_name_valid(env$input[[id_controller_name]]), str_message_name),
          shiny::need(rctval_source[[comp_source]], str_message_source)
        )

        shinyjs::enable(id_controller_add)

        cat("Source item is ready to be added to list.")
      })

    env$output[[id_view_members]] <-
      shiny::renderPrint({

        str_message_dest_empty <- "List is empty."

        shiny::validate(
          shiny::need(length(names(rctval_dest)) > 0, str_message_dest_empty)
        )

        str_message <- paste0(
          "Members of list: ",
          paste(rctval_names(rctval_dest), collapse = ", ")
        )

        cat(str_message)
      })

    observeEvent(
      eventExpr = env$input[[id_controller_add]],
      handlerExpr = {
        rctval_dest[[env$input[[id_controller_name]]]] <- rctval_source[[comp_source]]
      }
    )

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )
}
