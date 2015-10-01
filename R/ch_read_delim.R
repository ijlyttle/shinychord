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
#'  \item{\code{tz_parse}}{\code{shiny::selectInput used to select the timezone used to parse}}
#'  \item{\code{tz_parse_model}}{\code{shinyBS::bsModal used for timezone help}}
#'  \item{\code{tz_display}}{\code{shiny::selectInput used to select the timezone to display}}
#'  \item{\code{tz_display_model}}{\code{shinyBS::bsModal used for timezone help}}
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
#' @param id        character, tag to prepend to the input and output id's
#' @param defaults  list, default values for delimiter and decimal_mark
#'
#' @return list containing \code{ui_controller}, \code{ui_view}, and \code{server_model}
#' @export
#'
ch_read_delim <- function(id, defaults = list(delim = ",", decimal_mark = ".")){

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
      selected = defaults$delim
    )

  # specify decimal
  id_controller_decimal_mark <- id_name("controller", "decimal_mark")
  ui_controller$decimal_mark <-
    shiny::selectizeInput(
      inputId = id_controller_decimal_mark,
      label = "Decimal mark",
      choices = c(Point = ".", Comma = ","),
      selected = defaults$decimal_mark
    )

  # specify timezones
  tz_choice <- c("UTC", lubridate::olson_time_zones())

  # timezone file
  id_controller_tz_parse <- id_name("controller", "tz_parse")
  id_controller_tz_parse_link <- id_name("controller", "tz_parse", "link")
  id_controller_tz_parse_modal <- id_name("controller", "tz_parse", "modal")
  ui_controller$tz_parse <-
    shiny::selectizeInput(
      inputId = id_controller_tz_parse,
      label = htmltools::tags$span(
        htmltools::tags$a(
          id = id_controller_tz_parse_link,
          "Timezone to parse",
          shiny::icon("info-circle")
        )
      ),
      choices = tz_choice
    )

  ui_controller$tz_parse_modal <-
    shinyBS::bsModal(
      id = id_controller_tz_parse_modal,
      title = "Timezones",
      trigger = id_controller_tz_parse_link,
      size = "large",
      htmltools::HTML(
        readr::read_lines(
          system.file("help", "ch_read_delim", "tz.html", package = "shinychord")
        )
      )
    )

  # timezone
  id_controller_tz_display <- id_name("controller", "tz_display")
  id_controller_tz_display_link <- id_name("controller", "tz_display", "link")
  id_controller_tz_display_modal <- id_name("controller", "tz_display", "modal")
  ui_controller$tz_display <-
    shiny::selectizeInput(
      inputId = id_controller_tz_display,
      label = htmltools::tags$span(
        htmltools::tags$a(
          id = id_controller_tz_display_link,
          "Timezone to display",
          shiny::icon("info-circle")
        )
      ),
      choices = tz_choice
    )

  ui_controller$tz_display_modal <-
    shinyBS::bsModal(
      id = id_controller_tz_display_modal,
      title = "Timezones",
      trigger = id_controller_tz_display_link,
      size = "large",
      htmltools::HTML(
        readr::read_lines(
          system.file("help", "ch_read_delim", "tz.html", package = "shinychord")
        )
      )
    )

  ## ui_view ##
  ui_view <- shiny::tagList()

  # http://stackoverflow.com/questions/10374171/how-to-make-twitter-bootstraps-pre-blocks-scroll-horizontally
  fn_container <- function(...){
    htmltools::pre(
      ...,
      style = "overflow: auto; word-wrap: normal; white-space: pre;"
    )
  }

  # shows the raw text of the file (first few lines)
  id_view_text <- id_name("view", "text")
  ui_view$text <-
    shiny::htmlOutput(
      outputId = id_view_text,
      container = fn_container
    )

  # shows the first few lines of the parsed data-frame
  id_view_data <- id_name("view", "data")
  ui_view$data <-
    shiny::htmlOutput(
      outputId = id_view_data,
      container = fn_container
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

    rct_data <- reactive({

      df <-
        readr::read_delim(
          file = rct_txt(),
          delim = input[[id_controller_delim]],
          locale = readr::locale(
            decimal_mark = input[[id_controller_decimal_mark]],
            tz = input[[id_controller_tz_parse]]
          )
        )

      df <- df_with_tz(df, tz = input[[id_controller_tz_display]])

      shiny::validate(
        shiny::need(is.data.frame(df), "No data")
      )

      df
    })


    # observers
    shiny::observeEvent(
      eventExpr = input[[id_controller_tz_parse]],
      handlerExpr = {
        shiny::updateSelectInput(
          session = session,
          inputId = id_controller_tz_display,
          selected = input[[id_controller_tz_parse]]
        )
      }
    )

    shiny::observe({
      rctval_data[[item_data]] <- rct_data()
    })

    # outputs

    # sets the output for the raw text
    output[[id_view_text]] <-
      renderUI({

        shinyjs::disable(id_controller_delim)
        shinyjs::disable(id_controller_decimal_mark)
        shinyjs::disable(id_controller_tz_parse)
        shinyjs::disable(id_controller_tz_display)

        shiny::validate(
          shiny::need(rct_txt(), "File did not load properly")
        )

        shinyjs::enable(id_controller_delim)
        shinyjs::enable(id_controller_decimal_mark)
        shinyjs::enable(id_controller_tz_parse)
        shinyjs::enable(id_controller_tz_display)

        h <- rct_txt()
        h <- readr::read_lines(h, n_max = 7)
        h <- paste(h, collapse = "<br/>")
        h <- htmltools::HTML(h)

        h
      })

    # sets the output for the parsed dataframe
    output[[id_view_data]] <-
      renderUI({

        h <-
          devtools::with_options(
            list(width = 10000, dpylr.width = Inf, dplyr.print_min = 6),
            capture.output(print(rct_data()))
          )
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
