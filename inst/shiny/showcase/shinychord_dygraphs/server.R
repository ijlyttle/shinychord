
shinyServer(function(input, output, session) {

  rctval <- reactiveValues(
    data = wx_ames,
    dyopt = list(useDataTimezone = TRUE)
  )

  chord_dygraph$server_model(
    input, output, session,
    rctval_data = rctval, item_data = "data",
    item_dyopt = "dyopt"
  )

})
