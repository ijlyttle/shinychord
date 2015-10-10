shinyServer(function(input, output, session) {

  rctval_all <- reactiveValues(
    data = NULL
  )

  chord_read$server_model(
    input, output, session,
    rctval_data = rctval_all, item_data = "data"
  )
  chord_write$server_model(
    input, output, session,
    rctval_data = rctval_all, item_data = "data"
  )

})
