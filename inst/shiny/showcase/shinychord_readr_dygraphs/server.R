
shinyServer(function(input, output, session) {

  # The reactive values in rctval are:
  #
  # data   - NULL, dataframe - to be supplied by readr shinychord
  # dyopt  - list of dygraph options, see the dygraphs package
  #
  rctval <- reactiveValues(
    data = NULL,
    dyopt = list(useDataTimezone = TRUE)
  )

  # The readr server_model function has arguments:
  #
  # input, output, session - as passed into the shinyServer function
  # rctval_data, item_data - used to point to rctval$data (output)
  #
  chord_readr$server_model(
    input, output, session,
    rctval_data = rctval, item_data = "data"
  )

  # The dygraph server_model function has arguments:
  #
  # input, output, session - as passed into the shinyServer function
  # rctval_data, item_data - used to point to rctval$data (input)
  # rctval_dyopt, item_dyopt - used to point to rctval$dyopt (input)
  #
  chord_dygraph$server_model(
    input, output, session,
    rctval_data = rctval, item_data = "data",
    rctval_dyopt = rctval, item_dyopt = "dyopt"
  )

})
