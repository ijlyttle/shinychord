library("shiny")
library("shinychord")
library("dplyr")

shinyServer(function(input, output, session) {

  rctval_temp <- reactiveValues(
    csv = NULL
  )

  chord_tbl_csv$server_model(rctval = rctval_temp, item = "csv")
  chord_dygraph$server_model(rctval = rctval_temp, item = "csv")

})
