shinyServer(function(input, output, session) {

  rctval_temp <- reactiveValues(
    vec_time = c(1, 30)
  )

  chord_read_time$server_model(
    input, output, session,
    rctval_input = rctval_temp, item_input = "vec_time"
  )
})
