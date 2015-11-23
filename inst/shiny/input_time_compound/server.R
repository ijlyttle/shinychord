shinyServer(function(input, output, session) {

  rctval_temp <- reactiveValues(
    duration = lubridate::dhours(1) + lubridate::dminutes(30)
  )

  chord_read_time$server_model(
    input, output, session,
    rctval_input = rctval_temp, item_input = "duration"
  )
})
