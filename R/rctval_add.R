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
      label = "Item name",
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
  server_model <- function(rctval_source, item_source, rctval_dest){

    env = parent.frame()

    rct_status <- reactive({

      str_members <-
        ifelse(
          length(rctval_names(rctval_dest)) == 0,
          "List has no items",
          paste(
            "Items in list",
            paste(rctval_names(rctval_dest), collapse = ", "),
            sep = ": "
          )
        )

      str_name_quote <- paste0("\"", env$input[[id_controller_name]], "\"")

      str_status_ok <- paste("Item", str_name_quote, "can be added to list", sep = " ")

      # start by disabling all the controls
      shinyjs::disable(id_controller_name)
      shinyjs::disable(id_controller_add)

      # check to see if the item is not empty
      str_message_source <- "Item is empty"
      shiny::validate(
        shiny::need(rctval_source[[item_source]], str_message_source)
      )

      # passed the check, enable the name
      shinyjs::enable(id_controller_name)

      # check to see if the name is legal
      str_message_name <-
        paste(
          paste("Item name", str_name_quote, "not valid\n", sep = " "),
          "Item name:",
          "- must begin with non-numeric character",
          "- must not contain spaces",
          "- must not contain some special characters",
          sep = "\n"
        )

      is_name_valid <- function(x){
        x == make.names(x)
      }

      shiny::validate(
        shiny::need(is_name_valid(env$input[[id_controller_name]]), str_message_name)
      )

      # passed the check, enable the add button
      shinyjs::enable(id_controller_add)

      paste(str_members, str_status_ok, sep = "\n\n")
    })

    env$output[[id_view_status]] <- shiny::renderPrint(cat(rct_status()))

    outputOptions(env$output, id_view_status, suspendWhenHidden = FALSE)

    observeEvent(
      eventExpr = env$input[[id_controller_add]],
      handlerExpr = {
        rctval_dest[[env$input[[id_controller_name]]]] <- rctval_source[[item_source]]
      }
    )

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )
}
