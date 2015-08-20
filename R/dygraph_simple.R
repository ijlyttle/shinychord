#' dygraph_simple
#'
#' Creates a list of objects that can be placed in a shiny application
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
dygraph_simple <- function(id){

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  ## ui_controller ##
  ui_controller <- shiny::tagList()

  # time
  id_controller_time <- id_name("controller", "time")
  ui_controller$time <- shiny::uiOutput(id_controller_time)

  # Y1 axes
  id_controller_y1 <- id_name("controller", "y1")
  ui_controller$y1 <- shiny::uiOutput(id_controller_y1)

  # Y2 axes
  id_controller_y2 <- id_name("controller", "y2")
  ui_controller$y2 <- shiny::uiOutput(id_controller_y2)

  ## ui_view ##
  ui_view <- shiny::tagList()

  # dygraph
  id_view_dygraph <- id_name("controller", "dygraph")
  ui_view$dygraph <- dygraphs::dygraphOutput(id_view_dygraph)

  ## server_model ##
  server_model <- function(rctval, item, colors = NULL){

    env = parent.frame()

    # reactive values - these will hold the variable names for
    # the time-based and numeric columns of the data-frame
    var <- reactiveValues(
      chron = NULL,
      numeric = NULL
    )

    # at some point, have this work with dates, as well as date-times
    observe({
      var$chron <- df_names_inherits(rctval[[item]], "POSIXct")
      var$numeric <- df_names_inherits(rctval[[item]], "numeric")
    })

    sel <- reactiveValues(
      Y1 = NULL,
      Y2 = NULL
    )

    observe({
      sel$Y1 <- env$input[[id_controller_y1]]
      sel$Y2 <- env$input[[id_controller_y2]]
    })

    # select time variable
    env$output[[id_controller_time]] <-
      renderUI(
        selectizeInput(
          inputId = id_controller_time,
          label = "Time",
          choices = var$chron
        )
      )

    # select Y1 variable
    env$output[[id_controller_y1]] <-
      renderUI(
        selectizeInput(
          inputId = id_controller_y1,
          label = "Y1 axis",
          choices = setdiff(var$numeric, env$input[[id_controller_y2]]),
          multiple = TRUE,
          selected = sel$Y1
        )
      )

    # select Y2 variable
    env$output[[id_controller_y2]] <-
      renderUI(
        selectizeInput(
          inputId = id_controller_y2,
          label = "Y2 axis",
          choices = setdiff(var$numeric, env$input[[id_controller_y1]]),
          multiple = TRUE,
          selected = sel$Y2
        )
      )


    # dygraph
    env$output[[id_view_dygraph]] <- dygraphs::renderDygraph({

      shinyjs::disable(id_controller_time)
      shinyjs::disable(id_controller_y1)
      shinyjs::disable(id_controller_y2)

      str_message_nodata <- "No data"
      shiny::validate(
        shiny::need(rctval[[item]], str_message_nodata)
      )

      shinyjs::enable(id_controller_time)
      shinyjs::enable(id_controller_y1)
      shinyjs::enable(id_controller_y2)

      shiny::validate(
        shiny::need(NULL, "OK")
      )
    })
  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )

}
