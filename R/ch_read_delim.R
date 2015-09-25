#' Creates a collection of shiny objects to wrap the \code{readr::read_delim} function.
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
#'  \item{\code{file}}{\code{shiny::fileInput} used to choose the file to upload}
#'  \item{\code{delim}}{\code{shiny::selectInput} used to select the delimiter character }
#'  \item{\code{decimal_mark}}{\code{shiny::selectInput used to select the decimal-mark character}}
#'  \item{\code{tz_file}}{\code{shiny::selectInput used to select the timezone used in the file}}
#'  \item{\code{tz_local}}{\code{shiny::selectInput used to select the timezone at the location}}
#' }
#'
#' Note that the `tz` input will serve as an argument to `readr::locale`;
#' its meaning depends on the context of what it's parsing. See the readr
#' documentation for more details.
#'
#' The list \code{ui_view} will have members:
#'
#' \describe{
#'  \item{\code{text}}{\code{shiny::htmlOutput} showing a preview of the first few lines of the text file}
#'  \item{\code{data}}{\code{shiny::htmlOutput} showing a glimpse of the parsed dataframe}
#' }
#'
#' The function \code{server_model()} will be called from your server function.
#' Its arguments are:
#'
#' \describe{
#'  \item{\code{input, output, session}}{input, output, session values passed from your server function}
#'  \item{\code{rctval_data, item_data}}{
#'    \code{shiny::reactiveValues} object, character string.
#'    The parsed dataframe will be placed in \code{rctval_data[[item_data]]}.
#'  }
#' }
#'
#' @param id    character, tag to prepend to the input and output id's
#'
#' @return list containing \code{ui_controller}, \code{ui_view}, and \code{server_model}
#' @export
#'
ch_read_delim <- function(id){

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

  # specify decimal
  id_controller_decimal_mark <- id_name("controller", "decimal_mark")
  ui_controller$decimal_mark <-
    shiny::selectizeInput(
      inputId = id_controller_decimal_mark,
      label = "Decimal mark",
      choices = c(Point = ".", Comma = ","),
      selected = "."
    )

  # specify timezones
  tz_choice <- c("UTC", lubridate::olson_time_zones())

  # timezone file
  id_controller_tz_file <- id_name("controller", "tz_file")
  id_controller_tz_file_link <- id_name("controller", "tz_file", "link")
  id_controller_tz_file_modal <- id_name("controller", "tz_file", "modal")
  ui_controller$tz_file <-
    shiny::selectizeInput(
      inputId = id_controller_tz_file,
      label = htmltools::tags$span(
        htmltools::tags$a(
          id = id_controller_tz_file_link,
          "Timezone in file",
          shiny::icon("info-circle")
        ),
        shinyBS::bsModal(
          id = id_controller_tz_file_modal,
          title = "Timezone in file",
          trigger = id_controller_tz_file_link,
          size = "large",
          htmltools::HTML(
            readr::read_lines(
              system.file("help", "ch_read_delim", "tz_file.html", package = "shinychord")
            )
          )
        )
      ),
      choices = tz_choice
    )

  # timezone
  id_controller_tz_location <- id_name("controller", "tz_location")
  id_controller_tz_location_link <- id_name("controller", "tz_location", "link")
  id_controller_tz_location_modal <- id_name("controller", "tz_location", "modal")
  ui_controller$tz_local <-
    shiny::selectizeInput(
      inputId = id_controller_tz_location,
      label = htmltools::tags$span(
        htmltools::tags$a(
          id = id_controller_tz_location_link,
          "Timezone at location",
          shiny::icon("info-circle")
        ),
        shinyBS::bsModal(
          id = id_controller_tz_location_modal,
          title = "Timezone at location",
          trigger = id_controller_tz_location_link,
          size = "large",
          htmltools::HTML(
            readr::read_lines(
              system.file("help", "ch_read_delim", "tz_location.html", package = "shinychord")
            )
          )
        )
      ),
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
  ui_view$data <-
    shiny::htmlOutput(
      outputId = id_view_data,
      container = function(...){
        pre(..., style = "white-space: nowrap;")
      }
    )

  ## server_model ##
  server_model <- function(
    input, output, session,
    rctval_data, item_data
  ){

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
      rctval_data[[item_data]] <-
        readr::read_delim(
          file = rct_txt(),
          delim = input[[id_controller_delim]],
          locale = readr::locale(
            decimal_mark = input[[id_controller_decimal_mark]],
            tz = input[[id_controller_tz_file]]
          )
        )
    })

    # outputs

    # sets the output for the raw text
    output[[id_view_text]] <-
      renderUI({

        shinyjs::disable(id_controller_delim)
        shinyjs::disable(id_controller_decimal_mark)
        shinyjs::disable(id_controller_tz_file)
        shinyjs::disable(id_controller_tz_location)

        validate(
          need(rct_txt(), "File did not load properly")
        )

        shinyjs::enable(id_controller_delim)
        shinyjs::enable(id_controller_decimal_mark)
        shinyjs::enable(id_controller_tz_file)
        shinyjs::enable(id_controller_tz_location)

        h <- rct_txt()
        h <- readr::read_lines(h, n_max = 11)
        h <- paste(h, collapse = "<br/>")
        h <- htmltools::HTML(h)

        h
      })

    # sets the output for the parsed dataframe
    output[[id_view_data]] <-
      renderUI({

        shiny::validate(
          shiny::need(rctval_data[[item_data]], "No data")
        )

        h <- capture.output(dplyr::glimpse(rctval_data[[item_data]]))
        h <- paste(h, collapse = "<br/>")
        h <- htmltools::HTML(h)

        h
      })

  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )

}
