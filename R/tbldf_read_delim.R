#' tbldf_read
#'
#' Given some text, a delimiter, and tz attributes, return a data-frame
#'
#' @param file Passed to \code{readr::read_delim}. Either a path to a file, a connection, or literal data (either a single string or a raw vector).
#' @param delim Passed to \code{readr::read_delim}. Delimiter.
#' @param tz_file Timezone used to express the time in the file, see \code{lubridate::olson_time_zones()}
#' @param tz_location Timezone used to express the time at the location, see \code{lubridate::olson_time_zones()}
#'
#' @return \code{dplyr::tbl_df()} Dataframe-like structure
#' @export
#'
tbldf_read <- function(file, delim = ",", tz_file = "UTC", tz_location = "UTC"){

  # make a provisional parsing of the data-frame
  df <- readr::read_delim(file = file, delim = delim)

  # determine which columns are datetimes
  names_posixct <- df_names_inherits(df, "POSIXct")
  list_posixct <- as.list(names_posixct)
  names(list_posixct) <- names_posixct

  # run this only if there are datetime columns
  if (length(names_posixct) > 0){

    fn_parse <- function(x) {
      readr::col_datetime(tz = tz_file)
    }

    # set datetime parsers for the datetime columns
    list_parse_datetime <- lapply(list_posixct, fn_parse)

    # re-parse, using these parsers
    df <- readr::read_delim(file = file, delim = delim, col_types = list_parse_datetime)

    # set the timezone
    df <- df_set_tz(df, tz_location)

  }

  df
}

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
  server_model <- function(rctval, item){

    env = parent.frame()

    # reactive to read in the raw text from the file-specification input
    rct_txt <- reactive({

      shiny::validate(
        shiny::need(env$input[[id_controller_file]], "File not selected")
      )

      infile <- env$input[[id_controller_file]]$datapath

      readr::read_file(infile)
    })

    # this needs to be wrapped in a reactive expression
    # sets the reactive output
    observe({
      rctval[[item]] <-
        tbldf_read(
          file = rct_txt(),
          delim = env$input[[id_controller_delim]],
          tz_file = env$input[[id_controller_tzfile]],
          tz_location = env$input[[id_controller_tzloc]]
        )
    })

    # sets the output for the raw text
    env$output[[id_view_text]] <-
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
    env$output[[id_view_data]] <-
      renderPrint({

        shiny::validate(
          shiny::need(rctval[[item]], "No data")
        )

        print(rctval[[item]])
      })

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )

}
