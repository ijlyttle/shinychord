library("shiny")
library("shinychord")
library("dplyr")

shinyServer(function(input, output, session) {

  rctval_temp <- reactiveValues(
    list_data = NULL,
    data_csv = NULL,
    data_select = NULL
  )

  chord_data$server_model(
    input, output, session,
    rctval_data = rctval_temp, item_data = "data_csv"
  )

  chord_list_data_add$server_model(
    input, output, session,
    rctval_source = rctval_temp, item_source = "data_csv",
    item_list = "list_data"
  )

  chord_list_data_select$server_model(
    input, output, session,
    rctval_list = rctval_temp, item_list = "list_data",
    item_dest = "data_select"
  )
#
#   chord_list_tbl_remove$server_model(rctval = rctval_data)

})
