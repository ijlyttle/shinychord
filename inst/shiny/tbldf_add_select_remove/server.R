library("shiny")
library("shinychord")
library("dplyr")

shinyServer(function(input, output, session) {

  rctval_temp <- reactiveValues(
    data = NULL,
    list_data = NULL
  )

  chord_data$server_model(
    input, output, session,
    rctval_data = rctval_temp, item_data = "data"
  )

  chord_list_data_add$server_model(
    input, output, session,
    rctval_source = rctval_temp, item_source = "data",
    item_list = "list_data"
  )

#   chord_list_tbl_select$server_model(
#     rctval_source = rctval_data,
#     rctval_dest = rctval_temp,
#     item_dest = "select"
#   )
#
#   chord_list_tbl_remove$server_model(rctval = rctval_data)

})
