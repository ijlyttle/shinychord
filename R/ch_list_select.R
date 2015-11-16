#' Select an item from a list
#'
#' Creates a list of shiny objects to encapsulate the operation of selecting an item from a list.
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
#'  \item{\code{item}}{\code{shiny::selectInput} used to select the name of the item to be copied from the list}
#' }
#'
#' The list \code{ui_view} will have members:
#'
#' \describe{
#'  \item{\code{status}}{\code{shiny::verbatimTextOutput} showing what has been selected from the list}
#' }
#'
#' The function \code{server_model()} will be called from your server function.
#' Its arguments are:
#'
#' \describe{
#'  \item{\code{input, output, session}}{input, output, session values passed from your server function}
#'  \item{\code{rctval_list, item_list}}{
#'    \code{shiny::reactiveValues} object, character string
#'
#'    \code{rctval_list[[item_list]]} is expected to be a list,
#'    from which the selected item will be copied
#'  }
#'  \item{\code{rctval_dest, item_dest}}{
#'    \code{shiny::reactiveValues} object, character string
#'
#'    The default value for \code{rctval_dest} is \code{rctval_list}.
#'    \code{rctval_dest[[item_dest]]} is where the selected item will be copied.
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
#
ch_list_select <- function(id, item = "item", plural = NULL) {

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
  server_model <- function(
    input, output, session,
    rctval_list, item_list,
    rctval_dest = rctval_list, item_dest
  ){

    # reactives

    rct_choices <- reactive({

      choices <- names(rctval_list[[item_list]])

      # validate that the list is not empty
      shiny::validate(
        shiny::need(
          length(choices) > 0,
          str_message_empty <- paste("List has no", plural, sep = " ")
        )
      )

      choices
    })

    rct_selected <- reactive({

      # reactive dependency
      rct_choices()

      selected <- input[[id_controller_item]]

      selected
    })

    # observers
    shiny::observeEvent(
      eventExpr = rct_selected(),
      handlerExpr = {
        rctval_dest[[item_dest]] <- rctval_list[[item_list]][[rct_selected()]]
      }
    )

    # outputs
    output[[name_out(id_controller_item)]] <-
      shiny::renderUI({
        selectizeInput(
          inputId = id_controller_item,
          label = paste(stringr::str_to_title(item), "name", sep = " "),
          choices = rct_choices(),
          selected = rct_selected()
        )
      })

    output[[id_view_status]] <-
      shiny::renderText({

        str_message <-
          ifelse(
            is.null(rct_selected()),
            paste("No", item, "selected", sep = " "),
            paste(
              paste(stringr::str_to_title(item), "selected", sep = " "),
              rct_selected(),
              sep = ": "
            )
          )

        str_message
      })


    # reactive values - these will hold the variable names for
    # the time-based and numeric columns of the data-frame
#     sel <- reactiveValues(
#       item = NULL
#     )
#
#     observe({
#       sel$item <- env$input[[id_controller_item]]
#     })



  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )
}
