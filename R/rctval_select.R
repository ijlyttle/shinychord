#' rctval_select
#'
#' Creates a list of shiny objects to manage the selection of an item from
#' a list of reactive values.
#'
#' The returned list will contain:
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
rctval_select <- function(id) {

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  name_out <- function(x){
    paste(x, ".out.", sep = "_")
  }

  ## ui_controller ##
  ui_controller <- shiny::tagList()

  # select item to transfer
  id_controller_item <- id_name("controller", "item")
  ui_controller$item <- shiny::uiOutput(name_out(id_controller_item))

  ## ui_view ##
  ui_view <- shiny::tagList()

  # status of the possibility to add a member
  id_view_status <- id_name("view", "status")
  ui_view$status <- shiny::verbatimTextOutput(id_view_status)

  ## server_model ##
  server_model <- function(rctval_source, rctval_dest, item_dest){

    env = parent.frame()

    # reactive values - these will hold the variable names for
    # the time-based and numeric columns of the data-frame
    sel <- reactiveValues(
      item = NULL
    )

    observe({
      sel$item <- env$input[[id_controller_item]]
    })

    env$output[[name_out(id_controller_item)]] <-
      shiny::renderUI({
        selectizeInput(
          inputId = id_controller_item,
          label = "Item",
          choices = rctval_names(rctval_source),
          selected = sel$item
        )
      })

    env$output[[id_view_status]] <-
      shiny::renderPrint({

        # start by disabling all the controls
        shinyjs::disable(id_controller_item)

        rctval_dest[[item_dest]] <- NULL

        # validate that the list is not empty
        str_message_empty <- "List has no items"
        shiny::validate(
          shiny::need(length(rctval_names(rctval_source)) > 0, str_message_empty)
        )

        # passed the check, enable the selector
        shinyjs::enable(id_controller_item)

        str_message_item <- "No item selected"
        shiny::validate(
          shiny::need(env$input[[id_controller_item]], str_message_item)
        )

        rctval_dest[[item_dest]] <- rctval_source[[env$input[[id_controller_item]]]]

        str_message <- paste0(
          "Item selected: ",
          env$input[[id_controller_item]]
        )

        cat(str_message)

      })

    outputOptions(env$output, id_view_status, suspendWhenHidden = FALSE)

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )
}
