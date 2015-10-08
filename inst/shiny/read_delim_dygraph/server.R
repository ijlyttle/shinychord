library("shiny")
library("shinychord")
library("dplyr")
library("shinyBS")


shinyServer(function(input, output, session) {

  data_dygraphOptions <- list(
    useDataTimezone = TRUE,
    retainDateWindow = TRUE
  )

  rctval <- reactiveValues(
    data = NULL,
    dyopt = data_dygraphOptions
  )

  chord_tbl_csv$server_model(
    input, output, session,
    rctval_data = rctval, item_data = "data"
  )

  chord_dygraph$server_model(
    input, output, session,
    rctval_data = rctval, item_data = "data",
    item_dyopt = "dyopt"
  )

})
