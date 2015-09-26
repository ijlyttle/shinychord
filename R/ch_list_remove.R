#' Remove items from a list
#'
#' Creates a list of shiny objects to encapsulate the operation of removing an item from a list.
#'
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
#'  \item{\code{item}}{\code{shiny::selectInput} used to select the name of the item(s) to be remove from the list}
#'  \item{\code{remove}}{\code{shiny::actionButton} used to invoke the action of removing the item from the list}
#' }
#'
#' The list \code{ui_view} will have members:
#'
#' \describe{
#'   \item{\code{status}}{\code{shiny::verbatimTextOutput} showing what has been selected to be removed from the list}
#' }
#'
#' The function \code{server_model()} will be called from your server function.
#' Its arguments are:
#'
#' \describe{
#'   \item{\code{input, output, session}}{input, output, session values passed from your server function}
#'   \item{\code{rctval_list, item_list}}{
#'     \code{shiny::reactiveValues} object, character string
#'
#'     \code{rctval_list[[item_list]]} is expected to be a list,
#'     from which the selected item(s) will be removed.
#'   }
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
ch_list_remove <- function(id, item = "item", plural = NULL) {

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

  # select item(s) to remove
  id_controller_item <- id_name("controller", "item")
  ui_controller$item <- shiny::uiOutput(name_out(id_controller_item))

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

  # status of the possibility to remove a member
  id_view_status <- id_name("view", "status")
  ui_view$status <- shiny::verbatimTextOutput(id_view_status)

  ## server_model ##
  server_model <- function(
    input, output, session,
    rctval_list, item_list
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

      # put this above the "reactive" dependency,
      # so that the button disables if there are no choices
      shinyjs::disable(id_controller_remove)

      # reactive dependency
      rct_choices()

      selected <- input[[id_controller_item]]

      shiny::validate(
        shiny::need(selected, paste("No", plural, "selected for removal", sep = " "))
      )

      shinyjs::enable(id_controller_remove)

      selected
    })

    observeEvent(
      eventExpr = input[[id_controller_remove]],
      handlerExpr = {
        for (name in rct_selected()){
          rctval_list[[item_list]][[name]] <- NULL
        }
      }
    )

    # outputs
    output[[name_out(id_controller_item)]] <-
      shiny::renderUI({
        selectizeInput(
          inputId = id_controller_item,
          label = stringr::str_to_title(plural),
          choices = rct_choices(),
          selected = NULL,
          multiple = TRUE
        )
      })

    output[[id_view_status]] <-
      shiny::renderText({

        title <-
          ifelse(
            identical(length(rct_selected()), 1L),
            stringr::str_to_title(item),
            stringr::str_to_title(plural)
          )

        str_message <- paste(
          paste(title, "selected for removal", sep = " "),
          paste(rct_selected(), collapse = ", "),
          sep = ": "
        )

        str_message
      })

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )
}
