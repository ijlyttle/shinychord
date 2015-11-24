#' Creates a collection of shiny objects to wrap the \code{readr::write_delim} function.
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
#'  \item{\code{delim}}{\code{shiny::selectInput} used to select the delimiter character }
#'  \item{\code{tz_display}}{\code{shiny::selectInput used to select the timezone used to display}}
#'  \item{\code{tz_display_modal}}{\code{shinyBS::bsModal used for timezone help}}
#'  \item{\code{tz_name}}{\code{shiny::textInput used to name the file to be download}}
#'  \item{\code{download}}{\code{shiny::downloadButton used to launch the download}}
#' }
#'
#' The list \code{ui_view} will have members:
#'
#' \describe{
#'  \item{\code{data}}{\code{shiny::htmlOutput} showing the first few lines of the dataframe}
#'  \item{\code{text}}{\code{shiny::htmlOutput} showing the first few lines of the text file}
#'  \item{\code{status}}{\code{shiny::htmlOutput} showing the status of the download}
#' }
#'
#' The function \code{server_model()} will be called from your server function.
#' Its arguments are:
#'
#' \describe{
#'  \item{\code{input, output, session}}{input, output, session values passed from your server function}
#'  \item{\code{rctval_data, item_data}}{
#'    \code{shiny::reactiveValues} object, character string.
#'    The input dataframe will be taken from in \code{rctval_data[[item_data]]}.
#'  }
#' }
#'
#' @param id        character, tag to prepend to the input and output id's
#' @param defaults  list, default value for delimiter
#'
#' @return list containing \code{ui_controller}, \code{ui_view}, and \code{server_model}
#' @export
#
ch_write_delim <- function(id, defaults = list(delim = ",")){

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  name_out <- function(x){
    paste(x, ".out.", sep = "_")
  }

  ## ui_controller
  ui_controller <- shiny::tagList()

  # specify delimiter
  id_controller_delim <- id_name("controller", "delim")
  ui_controller$delim <-
    shiny::selectizeInput(
      inputId = id_controller_delim,
      label = "Delimiter",
      choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
      selected = defaults$delim
    )

  # # specify decimal
  # id_controller_decimal_mark <- id_name("controller", "decimal_mark")
  # ui_controller$decimal_mark <-
  #   shiny::selectizeInput(
  #     inputId = id_controller_decimal_mark,
  #     label = "Decimal mark",
  #     choices = c(Point = ".", Comma = ","),
  #     selected = defaults$decimal_mark
  #   )

  # specify filename
  id_controller_filename <- id_name("controller", "filename")
  ui_controller$filename <-
    shiny::textInput(
      inputId = id_controller_filename,
      label = "Filename",
      value = "data.csv"
    )

  # download button
  id_controller_download <- id_name("controller", "download")
  ui_controller$download <-
    shiny::downloadButton(
      outputId = name_out(id_controller_download),
      label = "Download",
      class = "btn-primary"
    )

  ## ui_view ##
  ui_view <- shiny::tagList()

  # shows the first few lines of the data-frame
  id_view_data <- id_name("view", "data")
  ui_view$data <-
    shiny::htmlOutput(
      outputId = id_view_data,
      container = pre_scroll
    )

  # shows the raw text of the file (first few lines)
  id_view_text <- id_name("view", "text")
  ui_view$text <-
    shiny::htmlOutput(
      outputId = id_view_text,
      container = pre_scroll
    )

  # shows the raw text of the file (first few lines)
  id_view_status <- id_name("view", "status")
  ui_view$status <-
    shiny::htmlOutput(
      outputId = id_view_status,
      container = pre_scroll
    )

  ## server_model ##
  server_model <- function(
    input, output, session,
    rctval_data, item_data
  ){

    # reactives
    rct_data <- reactive({

      df <- rctval_data[[item_data]]

      shiny::validate(
        shiny::need(is.data.frame(df), "No data")
      )

      dplyr::tbl_df(df)
    })

    rct_txt <- reactive({

      shinyjs::disable(id_controller_delim)
      shinyjs::disable(id_controller_filename)

      shiny::validate(
        shiny::need(rct_data(), "No data")
      )

      shinyjs::enable(id_controller_delim)
      shinyjs::enable(id_controller_filename)

      txt <-
        readr::write_delim(
          x = rct_data(),
          path = "",
          delim = input[[id_controller_delim]]
        )

      txt
    })

    rct_filename <- reactive({

      shinyjs::disable(name_out(id_controller_download))

      # just for the reactive dependency
      rct_data()

      shiny::validate(
        shiny::need(
          input[[id_controller_filename]],
          "Need a valid filename"
        )
      )

      shinyjs::enable(name_out(id_controller_download))

      input[[id_controller_filename]]
    })


    # outputs

    # sets the output for the input dataframe
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

    # sets the output for the raw text
    output[[id_view_text]] <-
      renderUI({

        h <- rct_txt()
        h <- readr::read_lines(h, n_max = 7)
        h <- paste(h, collapse = "<br/>")
        h <- htmltools::HTML(h)

        h
      })

    # sets the output for the status
    output[[id_view_status]] <-
      renderUI({

        paste(
          "Ready to download file",
          paste0("\"", rct_filename(), "\""),
          sep = ": "
        )
      })

    # do the download
    output[[name_out(id_controller_download)]] <-
      shiny::downloadHandler(
        filename = rct_filename,
        content = function(con){
          writeLines(rct_txt(), con, sep = "\r\n")
        },
        contentType = "text/csv"
      )

  }



  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )

}
