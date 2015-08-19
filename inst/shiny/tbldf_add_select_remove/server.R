library("shiny")
library("shinychord")
library("dplyr")

shinyServer(function(input, output, session) {

  rctval_temp <- reactiveValues(
    csv = NULL,
    select = NULL
  )

  rctval_data <- reactiveValues()

  chord_tbl$server_model(rctval = rctval_temp, item = "csv")

  chord_list_tbl_add$server_model(
    rctval_source = rctval_temp,
    item_source = "csv",
    rctval_dest = rctval_data
  )

  chord_list_tbl_select$server_model(
    rctval_source = rctval_data,
    rctval_dest = rctval_temp,
    item_dest = "select"
  )

  chord_list_tbl_remove$server_model(rctval = rctval_data)

  observe(print(rctval_temp$select))

})
