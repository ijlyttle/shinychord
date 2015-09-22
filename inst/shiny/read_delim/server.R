library("shiny")
library("shinychord")
library("dplyr")

shinyServer(function(input, output, session) {

  rctval_temp <- reactiveValues(
    a = NULL,
    b = NULL
  )

  chord_tbl_1$server_model(
    input, output, session,
    rctval_data = rctval_temp, item_data = "a"
  )
  chord_tbl_2$server_model(
    input, output, session,
    rctval_data = rctval_temp, item_data = "b"
  )

  observe(print(rctval_temp$a))
  observe(print(rctval_temp$b))


})
