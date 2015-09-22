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

  name_out <- function(x){
    paste(x, ".out.", sep = "_")
  }

  ## ui_controller ##
  ui_controller <- shiny::tagList()

  # time
  id_controller_time <- id_name("controller", "time")
  ui_controller$time <- shiny::uiOutput(name_out(id_controller_time))

  # Y1 axes
  id_controller_y1 <- id_name("controller", "y1")
  ui_controller$y1 <- shinyjs::disabled(shiny::uiOutput(name_out(id_controller_y1)))

  # Y2 axes
  id_controller_y2 <- id_name("controller", "y2")
  ui_controller$y2 <- shiny::uiOutput(name_out(id_controller_y2))

  ## ui_view ##
  ui_view <- shiny::tagList()

  # dygraph
  id_view_dygraph <- id_name("view", "dygraph")
  ui_view$dygraph <- dygraphs::dygraphOutput(id_view_dygraph)

  ## server_model ##
  server_model <- function(
    input, output, session,
    rctval_data, item_data,
    rctval_dyopt = rctval_data, item_dyopt
  ){

    # reactives
    rct_data <- reactive({

      shiny::validate(
        shiny::need(rctval_data[[item_data]], "No data")
      )

      rctval_data[[item_data]]
    })

    rct_var_time <- reactive({
      df_names_inherits(rct_data(), c("POSIXct"))
    })

    rct_var_num <- reactive({
      df_names_inherits(rct_data(), c("numeric", "integer"))
    })

    selection <- reactiveValues(
      time = NULL,
      Y1 = NULL,
      Y2 = NULL
    )

    # observers
    shiny::observe({
      selection$time <- input[[id_controller_time]]
      selection$Y1 <- input[[id_controller_y1]]
      selection$Y2 <- input[[id_controller_y2]]
    })

    # outputs

    # select time variable
    output[[name_out(id_controller_time)]] <-
      renderUI(
        selectizeInput(
          inputId = id_controller_time,
          label = "Time",
          choices = rct_var_time(),
          selected = selection$time
        )
      )

    # select Y1 variable
    output[[name_out(id_controller_y1)]] <-
      renderUI(
        selectizeInput(
          inputId = id_controller_y1,
          label = "Y1 axis",
          choices = setdiff(rct_var_num(), input[[id_controller_y2]]),
          multiple = TRUE,
          selected = selection$Y1
        )
      )

    # select Y2 variable
    output[[name_out(id_controller_y2)]] <-
      renderUI(
        selectizeInput(
          inputId = id_controller_y2,
          label = "Y2 axis",
          choices = setdiff(rct_var_num(), input[[id_controller_y1]]),
          multiple = TRUE,
          selected = selection$Y2
        )
      )

    # dygraph
    output[[id_view_dygraph]] <- dygraphs::renderDygraph({

      var_time <- selection$time
      var_y1 <- selection$Y1
      var_y2 <- selection$Y2
      var_yall <- c(var_y1, var_y2)

      shiny::validate(
        shiny::need(var_time, "No time-variable"),
        shiny::need(var_yall, "No y-variables")
      )

      # create the mts object
      vec_time <- rct_data()[[var_time]]
      df_num <- rct_data()[, var_yall]

      dy_xts <- xts::xts(df_num, order.by = vec_time, lubridate::tz(vec_time))

      dyg <- dygraphs::dygraph(dy_xts)

      dyopt <- function(...){
        dygraphs::dyOptions(dyg, ...)
      }
      dyg <- do.call(dyopt, rctval_dyopt[[item_dyopt]])

      dyg <- dygraphs::dyAxis(dyg, "x", label = var_time)
      dyg <- dygraphs::dyAxis(dyg, "y", label = paste(var_y1, collapse = ", "))
      dyg <- dygraphs::dyAxis(dyg, "y2", label = paste(var_y2, collapse = ", "))

      # put stuff on y2 axis
      for(v in var_y2) {
        dyg <- dygraphs::dySeries(dyg, v, axis = "y2")
      }

      dyg
    })

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )

}
