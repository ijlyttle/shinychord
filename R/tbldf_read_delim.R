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

  # specify separator
  id_controller_sep <- id_name("controller", "sep")
  ui_controller$sep <-
    shiny::selectizeInput(
      inputId = id_controller_sep,
      label = "Separator",
      choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
      selected = ";"
    )

  # specify timezones
  tz_choice <- c("UTC", lubridate::olson_time_zones())

  id_controller_tzfile <- id_name("controller", "tzfile")
  ui_controller$tzfile <-
    shiny::selectizeInput(
      inputId = id_controller_tzfile,
      label = "Timezone in file",
      choices = tz_choice
    )

  id_controller_tzloc <- id_name("controller", "tzloc")
  ui_controller$tzloc <-
    shiny::selectizeInput(
      inputId = id_controller_tzloc,
      label = "Timezone at location",
      choices = tz_choice
    )

  ## ui_view ##
  ui_view <- shiny::tagList()

  id_view_text <- id_name("view", "text")
  ui_view$text <-
    shiny::htmlOutput(
      outputId = id_view_text,
      container = function(...){
        pre(..., style = "white-space: nowrap;")
      }
    )

  id_view_data <- id_name("view", "data")
  ui_view$data <- shiny::verbatimTextOutput(id_view_data)

  ## server_model ##
  server_model <- function(rctval, item){

    env = parent.frame()

    rct_txt <- reactive({

      shiny::validate(
        shiny::need(env$input[[id_controller_file]], "File not selected")
      )

      infile <- env$input[[id_controller_file]]$datapath

      readr::read_file(infile)
    })

    env$output[[id_view_text]] <-
      renderUI({

        shinyjs::disable(id_controller_sep)
        shinyjs::disable(id_controller_tzfile)
        shinyjs::disable(id_controller_tzloc)

        validate(
          need(rct_txt(), "File did not load properly")
        )

        shinyjs::enable(id_controller_sep)
        shinyjs::enable(id_controller_tzfile)
        shinyjs::enable(id_controller_tzloc)

        h <- rct_txt()
        h <- readr::read_lines(h, n_max = 11)
        h <- paste(h, collapse = "<br/>")
        h <- htmltools::HTML(h)

        h
      })

    observe({

      # make a provisional parsing of the data-frame
      df_tmp <-
        readr::read_delim(
          file = rct_txt(),
          delim = env$input[[id_controller_sep]]
        )

      # determine which columns are datetimes
      names_posixct <- df_names_inherits(df_tmp, "POSIXct")
      list_posixct <- list(names_posixct)
      names(list_posixct) <- names_posixct

      list_parse_datetime <-
        lapply(
          list_posixct,
          function(x){
            readr::col_datetime(tz = env$input[[id_controller_tzfile]])
          }
        )

      df_new <-
        readr::read_delim(
          file = rct_txt(),
          delim = env$input[[id_controller_sep]],
          col_types = list_parse_datetime
        )

      rctval[[item]] <- df_set_tz(df_new, env$input[[id_controller_tzloc]])
    })

    env$output[[id_view_data]] <-
      renderPrint({

        shiny::validate(
          shiny::need(rctval[[item]], "No data")
        )

        print(rctval[[item]])
      })

    outputOptions(env$output, id_view_data, suspendWhenHidden = FALSE)

  }


  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )

}
