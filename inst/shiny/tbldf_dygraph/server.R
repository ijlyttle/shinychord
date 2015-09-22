library("shiny")
library("shinychord")
library("dplyr")

shinyServer(function(input, output, session) {

  data_dygraphOptions <- list(
    useDataTimezone = TRUE,
    retainDateWindow = TRUE
  )

  rctval <- reactiveValues(
    csv = NULL,
    dyopt = data_dygraphOptions
  )

  chord_tbl_csv$server_model(
    input, output, session,
    rctval = rctval,
    item_data = "csv"
  )

#  observe(str(rctval_temp$csv))

  chord_dygraph$server_model(
    input, output, session,
    rctval_data = rctval,
    item_data = "csv",
    item_dyopt = "dyopt"
  )

})
