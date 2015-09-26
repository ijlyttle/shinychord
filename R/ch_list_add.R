#' Add an item to a list
#'
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
#'    \code{shiny::reactiveValues} object, character string
#'
#'    \code{rctval_source[[item_source]]} an object of some sort to be added to the list.
#'  }
#'  \item{\code{rctval_list, item_list}}{
#'    \code{shiny::reactiveValues} object, character string
#'
#'    The default value for \code{rctval_list} is \code{rctval_source}.
#'    \code{rctval_list[[item_list]]} is expected to be a list, to which the object will be added
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
ch_list_add <- function(id, item = "item", plural = NULL) {

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  if (is.null(plural)){
    plural <- paste0(item, "s")
  }

  ## ui_controller ##
  ui_controller <- shiny::tagList()

  # name of the new element
  id_controller_name <- id_name("controller", "name")
  ui_controller$name <-
    shiny::textInput(
      inputId = id_controller_name,
      label = paste(stringr::str_to_title(item), "name", sep = " "),
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

  # status
  id_view_status <- id_name("view", "status")
  ui_view$status <- shiny::verbatimTextOutput(id_view_status)

  ## server_model ##
  server_model <- function(
    input, output, session,
    rctval_source, item_source,
    rctval_list = rctval_source, item_list
  ){

    # reactives

    # source item
    rct_source <- reactive({

      # disable the controls
      shinyjs::disable(id_controller_name)
      shinyjs::disable(id_controller_add)

      # check to see that item is not empty
      str_message_source <- paste(stringr::str_to_title(item), "is empty", sep = " ")
      shiny::validate(
        shiny::need(rctval_source[[item_source]], str_message_source)
      )

      # passed the check, enable the name
      shinyjs::enable(id_controller_name)

      rctval_source[[item_source]]
    })

    # name to add to desination list
    rct_name_new <- reactive({

      # disable the add button
      shinyjs::disable(id_controller_add)

      # put in a reactive dependency
      # to keep the add button from disabling
      rct_source()

      name_new <- input[[id_controller_name]]

      str_message_name <-
        paste(
          paste(
            stringr::str_to_title(item),
            "name:",
            paste0("\"", name_new, "\""),
            "not valid\n",
            sep = " "
          ),
          "- must begin with non-numeric character",
          "- must not contain spaces",
          "- must not contain some special characters",
          sep = "\n"
        )

      # do we need a valid name?
      is_name_valid <- function(x){
        identical(x, make.names(x))
      }

      shiny::validate(
        shiny::need(is_name_valid(input[[id_controller_name]]), str_message_name)
      )

      # enable the add button
      shinyjs::enable(id_controller_add)

      name_new
    })

    # names in the destination list
    rct_name_list <- reactive({
      names(rctval_list[[item_list]])
    })


    # observers
    observeEvent(
      eventExpr = input[[id_controller_add]],
      handlerExpr = {
        rctval_list[[item_list]][[rct_name_new()]] <- rct_source()
      }
    )

    # outputs
    output[[id_view_status]] <-
      shiny::renderText({

        # just for the reactive dependency
        rct_source()

        str_list_members <-
          ifelse(
            identical(length(rct_name_list()), 0L),
            paste("List has no", plural, sep = " "),
            paste(
              paste(stringr::str_to_title(plural), "in list", sep = " "),
              paste(rct_name_list(), collapse = ", "),
              sep = ": "
            )
          )

        str_new_member <- paste(
          stringr::str_to_title(item),
          paste0("\"", rct_name_new(), "\""),
          "can be added to list",
          sep = " "
        )

        paste(str_list_members, str_new_member, sep = "\n\n")
      })

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )
}
