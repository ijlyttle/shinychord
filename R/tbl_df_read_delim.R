#' tbl_df_read_delim
#'
#' Creates a collection of shiny objects to manage the parsing of a delimited file.
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
#'
tbl_df_read_delim <- function(id){

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  ## ui_controller ##
  ui_controller <- shiny::tagList()
  id_controller_file <- id_name("controller", "file")

  ui_controller[[id_controller_file]] <-
    fileInput(
      inputId = id_controller_file,
      label = "File",
      accept = c("text/csv", ".csv", "text/comma-separated-values", "text/plain")
    )

  ## ui_view ##
  ui_view <- shiny::tagList()
  id_view_text <- id_name("view", "text")
  id_view_data <- id_name("view", "data")

  ui_view[[id_view_text]] <-
    htmlOutput(
      outputId = id_view_text,
      container = function(...){
        pre(..., style = "white-space: nowrap;")
      }
    )

  ui_view[[id_view_data]] <- verbatimTextOutput(id_view_data)


  ## server_model ##
  server_model <- function(rct_val, comp){

    env = parent.frame()

    rct_txt <- reactive({

      validate(
        need(env$input[[id_controller_file]], "File not selected")
      )

      infile <- env$input[[id_controller_file]]$datapath

      readr::read_file(infile)
    })

    env$output[[id_view_text]] <-
      renderUI({

        validate(
          need(rct_txt(), "File did not load properly")
        )

        h <- rct_txt()
        h <- readr::read_lines(h, n_max = 11)
        h <- paste(h, collapse = "<br/>")
        h <- htmltools::HTML(h)

        h
      })

    observe(
      rct_val[[comp]] <- readr::read_delim(rct_txt(), delim = ";")
    )

    env$output[[id_view_data]] <-
      renderPrint({

        validate(
          need(rct_val[[comp]], "No data")
        )

        print(rct_val[[comp]])
      })
  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )

}
