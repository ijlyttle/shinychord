library("shiny")
library("shinychord")
library("dplyr")

shinyServer(function(input, output, session) {

  rctval_temp <- reactiveValues(
    csv = NULL
  )

  rctval_data <- reactiveValues()

  chord_tbl$server_model(rctval = rctval_temp, item = "csv")

  chord_list_tbl_add$server_model(
    rctval_source = rctval_temp,
    item_source = "a",
    rctval_dest = rctval_data
  )

  chord_list_tbl_remove$server_model(rctval = rctval_data)

})
