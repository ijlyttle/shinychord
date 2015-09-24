#' Creates a list of shiny objects to encapsulate the operation of
#' Creates a list of shiny objects to encapsulate the operation of adding an item to a list.
#'
#' The list of shiny objects will contain:
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
#'  \item{\code{name}}{\code{shiny::textInput} used to specify the name of the item to be added to the list}
#'  \item{\code{add}}{\code{shiny::actionButton} used to invoke the action of adding the item to the list}
#' }
#'
#' The list \code{ui_view} will have members:
#'
#' \describe{
#'  \item{\code{status}}{\code{shiny::verbatimTextOutput} showing if it is possible to add to the list}
#' }
#'
#' The function \code{server_model()} will be called from your server function.
#' Its arguments are:
#'
#' \describe{
#'  \item{\code{input, output, session}}{input, output, session values passed from your server function}
#'  \item{\code{rctval_source, item_source}}{
#'    \code{shiny::reactiveValues} object, character string.
#'    \code{rctval_source[[item_source]]} an object of some sort to be added to the list.
#'  }
#'  \item{\code{rctval_list, item_list}}{
#'    \code{shiny::reactiveValues} object, character string.
#'    The default value for \code{rctval_list} is \code{rctval_source}.
#'    \code{rctval_list[[rctval_list]]} is expected to be a list
#'  }
#' }
#'
#' @param id      character, tag to prepend to the input and output id's
#' @param item    character, for the ui, what to call the item being added - default is "item"
#' @param plural  character, for the ui, what to call the plural of the item being added
#'                  - default is \code{paste0(item, "s")}
#'
#' @return list containing \code{ui_controller}, \code{ui_view}, and \code{server_model}
#' @export
#'
rctval_select <- function(id, item = "item", plural = NULL) {

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  name_out <- function(x){
    paste(x, ".out.", sep = "_")
  }

  if (is.null(plural)){
    plural <- paste0(item, "s")
  }

  ## ui_controller ##
  ui_controller <- shiny::tagList()

  # select item to select
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
          label = paste(stringr::str_to_title(item), "name", sep = " "),
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
        str_message_empty <- paste("List has no", plural, sep = " ")
        shiny::validate(
          shiny::need(length(rctval_names(rctval_source)) > 0, str_message_empty)
        )

        # passed the check, enable the selector
        shinyjs::enable(id_controller_item)

        str_message_item <- paste("No", item, "selected", sep = " ")

        shiny::validate(
          shiny::need(env$input[[id_controller_item]], str_message_item)
        )

        rctval_dest[[item_dest]] <- rctval_source[[env$input[[id_controller_item]]]]

        str_message <- paste(
          paste(stringr::str_to_title(item), "selected", sep = " "),
          env$input[[id_controller_item]],
          sep = ": "
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
