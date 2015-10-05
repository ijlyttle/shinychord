
shinyServer(function(input, output, session) {

  # The reactive values in rctval are:
  #
  # data   - wx_ames, dataframe - weather in Ames, IA in Jan. 2014
  # dyopt  - list of dygraph options, see the dygraphs package
  #
  # wx_ames is included in the shinychord package
  #
  rctval <- reactiveValues(
    data = wx_ames,
    dyopt = list(useDataTimezone = TRUE)
  )

  # The server_model function has arguments:
  #
  # input, output, session - as passed into the shinyServer function
  # rctval_data, item_data - used to point to rctval$data
  # rctval_dyopt, item_dyopt - used to point to rctval$dyopt
  #
  chord_dygraph$server_model(
    input, output, session,
    rctval_data = rctval, item_data = "data",
    rctval_dyopt = rctval, item_dyopt = "dyopt"
  )

})
