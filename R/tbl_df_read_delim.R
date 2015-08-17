#' tbl_df_read_delim
#'
#' Creates a collection of shiny objects to manage the parsing of a delimited file.
#'
#' The list will contain:
#'
#' \itemize{
#'   \item \code{ui_control} \code{shiny::taglist} of control items
#'   \item \code{ui_response} \code{shing::taglist} of response items
#'   \item \code{srv_function} a function with reactive code - this function takes a \code{reactiveValues} as its arguement
#' }
#'
#' The list returned by the factory has to be available to both the ui and the server. If not using the \code{shinyApp}
#' formulation, perhaps \code{global.R} could be useful.
#'
#' The component \code{srv_function} will be called in the server function, using the
#' particular \code{reactiveValue} you wish to associate with the "thing".
#'
#' @param id    character, tag to prepend to the input and output id's
#'
#' @return list containing \code{ui_control}, \code{ui_response}, and \code{srv_function}
#'
tbl_df_read_delim <- function(id){

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  ## ui_control ##
  ui_control <- shiny::tagList()
  id_control_file <- id_name("control", "file")

  ui_control[[id_control_file]] <-
    fileInput(
      inputId = id_control_file,
      label = "File",
      accept = c("text/csv", ".csv", "text/comma-separated-values", "text/plain")
    )

  ## ui_response ##
  ui_response <- shiny::tagList()
  id_response_text <- id_name("response", "text")

  ui_response[[id_response_text]] <-
    htmlOutput(
      outputId = id_response_text,
      container = function(...){
        pre(..., style = "white-space: nowrap;")
      }
    )

  ## srv_function ##
  srv_function <- function(){

    env = parent.frame()

    rct_txt <- reactive({

      infile <- env$input[[id_control_file]]$datapath

      readr::read_file(infile)
    })

    env$output[[id_response_text]] <-
      renderUI({

        h <- rct_txt()
        h <- readr::read_lines(h, n_max = 7)
        h <- paste(h, collapse = "<br/>")
        h <- htmltools::HTML(h)

        h
      })

  }

  list(
    ui_control = ui_control,
    ui_response = ui_response,
    srv_function = srv_function
  )

}
