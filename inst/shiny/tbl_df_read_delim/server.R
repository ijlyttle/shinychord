library("shiny")
library("shinychord")
library("dplyr")

shinyServer(function(input, output, session) {

  rctval_data <- reactiveValues(
    a = NULL,
    b = NULL
  )

  chord_tbl_1$server_model(rctval_data, "a")
  chord_tbl_2$server_model(rctval_data, "b")

  observe(print(rctval_data$a))
  observe(print(rctval_data$b))

})
