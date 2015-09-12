library("shiny")
library("shinychord")
library("dplyr")

shinyServer(function(input, output, session) {

  rctval_temp <- reactiveValues(
    csv = NULL
  )

  data_dygraphOptions <- list(
    useDataTimezone = TRUE,
    retainDateWindow = TRUE
  )

  chord_tbl_csv$server_model(
    input, output, session,
    rctval = rctval_temp,
    item_data = "csv"
  )

#  observe(str(rctval_temp$csv))

  observe(df <- rctval_temp$csv)

  chord_dygraph$server_model(
    df,
    dygraph_options = data_dygraphOptions
  )

})
