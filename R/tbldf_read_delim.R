#' tbldf_read_delim
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
#' @export
#'
tbldf_read_delim <- function(id){

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  name_out <- function(x){
    paste(x, ".out.", sep = "_")
  }

  ## ui_controller ##
  ui_controller <- shiny::tagList()

  # specify file
  id_controller_file <- id_name("controller", "file")
  ui_controller$file <-
    shiny::fileInput(
      inputId = id_controller_file,
      label = "File",
      accept = c("text/csv", ".csv", "text/comma-separated-values", "text/plain")
    )

  # specify delimiter
  id_controller_delim <- id_name("controller", "delim")
  ui_controller$delim <-
    shiny::selectizeInput(
      inputId = id_controller_delim,
      label = "Delimiter",
      choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
      selected = ";"
    )

  # specify timezones
  tz_choice <- c("UTC", lubridate::olson_time_zones())

  # timezone as expressed in file
  id_controller_tzfile <- id_name("controller", "tzfile")
  ui_controller$tzfile <-
    shiny::selectizeInput(
      inputId = id_controller_tzfile,
      label = "Timezone in file",
      choices = tz_choice
    )

  # timezone at location
  id_controller_tzloc <- id_name("controller", "tzloc")
  ui_controller$tzloc <-
    shiny::selectizeInput(
      inputId = id_controller_tzloc,
      label = "Timezone at location",
      choices = tz_choice
    )

  ## ui_view ##
  ui_view <- shiny::tagList()

  # shows the raw text of the file (first few lines)
  id_view_text <- id_name("view", "text")
  ui_view$text <-
    shiny::htmlOutput(
      outputId = id_view_text,
      container = function(...){
        pre(..., style = "white-space: nowrap;")
      }
    )

  # shows the first few lines of the parsed data-frame
  id_view_data <- id_name("view", "data")
  ui_view$data <- shiny::verbatimTextOutput(id_view_data)

  ## server_model ##
  server_model <- function(input, output, session, rctval, item_data){

    # reactives

    # reactive to read in the raw text from the file-specification input
    rct_txt <- reactive({

      shiny::validate(
        shiny::need(input[[id_controller_file]], "File not selected")
      )

      infile <- input[[id_controller_file]]$datapath

      readr::read_file(infile)
    })


    # observers

    observe({
      rctval[[item_data]] <-
        readr::read_delim(
          file = rct_txt(),
          delim = input[[id_controller_delim]],
          locale = readr::locale(tz = input[[id_controller_tzfile]])
        )
    })

    # outputs

    # sets the output for the raw text
    output[[id_view_text]] <-
      renderUI({

        shinyjs::disable(id_controller_delim)
        shinyjs::disable(id_controller_tzfile)
        shinyjs::disable(id_controller_tzloc)

        validate(
          need(rct_txt(), "File did not load properly")
        )

        shinyjs::enable(id_controller_delim)
        shinyjs::enable(id_controller_tzfile)
        shinyjs::enable(id_controller_tzloc)

        h <- rct_txt()
        h <- readr::read_lines(h, n_max = 11)
        h <- paste(h, collapse = "<br/>")
        h <- htmltools::HTML(h)

        h
      })

    # sets the output for the parsed dataframe
    output[[id_view_data]] <-
      renderPrint({

        shiny::validate(
          shiny::need(rctval[[item_data]], "No data")
        )

        print(rctval[[item_data]])
      })

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )

}
