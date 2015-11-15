shinyServer(function(input, output, session) {

  rctval_temp <- reactiveValues(
    time = NULL
  )

  chord_read_time$server_model(
    input, output, session,
    rctval_source = rctval_temp, item_source = "time"
  )
})
