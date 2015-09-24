#' Creates a collection of shiny objects to add an item to a list of items.
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
#'  \item{\code{time}}{\code{shiny::selectInput} used to choose the variable for the x axis}
#'  \item{\code{y1}}{\code{shiny::selectInput} used to choose the variable for the y1 axis}
#'  \item{\code{y2}}{\code{shiny::selectInput} used to choose the variable for the y2 axis}
#' }
#'
#' The list \code{ui_view} will have members:
#'
#' \describe{
#'  \item{\code{dygraph}}{\code{dygraphs::dygraphOutput} showing a preview of the first few lines of the text file}
#' }
#'
#' The function \code{server_model()} will be called from your server function.
#' Its arguments are:
#'
#' \describe{
#'  \item{\code{input, output, session}}{input, output, session values passed from your server function}
#'  \item{\code{rctval_data, item_data}}{
#'    \code{shiny::reactiveValues} object, character string.
#'    \code{rctval_data[[item_data]]} is expected to be a dataframe, input for dygraph.
#'  }
#'  \item{\code{rctval_dyopt, item_dyopt}}{
#'    \code{shiny::reactiveValues} object, character string.
#'    The default value for \code{rctval_dyopt} is \code{rctval_dyopt}.
#'    \code{rctval_data[[item_data]]} is expected to be a list of dygraph options,
#'    see documentation for \code{dygraphs::dyOptions}
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
    rctval_dest = rctval_source, item_dest
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

      str_message_name <-
        paste(
          paste(
            paste(stringr::str_to_title(item), "name:", sep = " "),
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

      input[[id_controller_name]]
    })

    # names in the destination list
    rct_name_dest <- reactive({
      names(rctval_dest[[item_dest]])
    })


    # observers
    observeEvent(
      eventExpr = input[[id_controller_add]],
      handlerExpr = {
        rctval_dest[[item_dest]][[rct_name_new()]] <- rct_source()
      }
    )

    # outputs
    output[[id_view_status]] <-
      shiny::renderText({

        # just for the reactive dependency
        rct_source()

        str_list_members <-
          ifelse(
            identical(length(rct_name_dest()), 0) | is.null(rct_name_dest()),
            paste("List has no", plural, sep = " "),
            paste(
              paste(stringr::str_to_title(plural), "in list", sep = " "),
              paste(rct_name_dest(), collapse = ", "),
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
