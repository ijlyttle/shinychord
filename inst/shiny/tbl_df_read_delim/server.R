library("shiny")
library("shinychord")
library("dplyr")

shinyServer(function(input, output, session) {

  rctval_temp <- reactiveValues(
    a = NULL,
    b = NULL
  )

  rctval_data <- reactiveValues()

  chord_tbl_1$server_model(rctval_temp, "a")
  chord_list_tbl_1_add$server_model(rctval_temp, "a", rctval_data)
  chord_list_tbl_1_remove$server_model(rctval_data)

  chord_tbl_2$server_model(rctval_temp, "b")

  observe(print(rctval_temp$a))
  observe(print(rctval_temp$b))
  observe(print(rctval_names(rctval_data)))

})
