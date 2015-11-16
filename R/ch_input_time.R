ch_input_time <- function(id, title = "", default = "24:00") {

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  ## ui_controller ##
  ui_controller <- shiny::tagList()

  # text input for time
  id_controller_time <- id_name("controller", "time")
  ui_controller$name <-
    shiny::textInput(
      inputId = id_controller_time,
      label = stringr::str_to_title(title),
      value = default
    )

  ## ui_view ##
  ui_view <- shiny::tagList()

  # text view of time
  id_text_time <- id_name("view", "time")
  ui_view$status <- shiny::verbatimTextOutput(id_text_time)

  ## server_model ##
  server_model <- function(
    input, output, session,
    rctval_source, item_source
  ){
    ## reactives ##
    rct_raw_text <- reactive({
      raw_text = input[[id_controller_time]]

      shiny::validate(
        shiny::need(raw_text, "Please enter time")
      )

      raw_text
    })

    rct_parse_text <- reactive({
      text = rct_raw_text()

      if(!stringr::str_detect(text, ":")) {
        hour = as.numeric(text)
        min = NA
      } else {
        text = stringr::str_replace_all(text, "[^0-9:]", "")
        text_split = stringr::str_split(text, ":")[[1]]

        hour = as.numeric(text_split[1])
        min = as.numeric(text_split[2])
      }

      if(is.na(hour)) {
        return(c(NA, NA))
        #return(NA)
      }
      if(is.na(min)) {
        min = 0
      }

      shiny::validate(
        shiny::need(c(hour, min), "Please enter valid time")
      )

      return(c(hour, min))
      #return(hour + round(min/60))
    })

    ## observers ##
    observeEvent(
      eventExpr = rctval_source[[item_source]],
      handlerExpr = {
        rctval_source[[item_source]] <- rct_parse_text()
      }
    )

    ## outputs ##
    output[[id_text_time]] <- 
      shiny::renderText({
        #paste(rct_parse_text(), collapse = ":")
        rct_parse_text()
      })
  }

  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )
}