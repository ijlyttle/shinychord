library("shiny")
library("shinychord")

chord_dygraph = ch_dygraph(id = "dyg")

shinyApp(

  ui = fluidPage(
    titlePanel("dygraph demo"),
    sidebarLayout(
      sidebarPanel(chord_dygraph$ui_controller),
      mainPanel(chord_dygraph$ui_view)
    )
  ),

  server = function(input, output, session){

    rctval <- reactiveValues(
      data = wx_ames,
      dyopt = list(useDataTimezone = TRUE)
    )

    chord_dygraph$server_model(
      input, output, session,
      rctval_data = rctval, item_data = "data",
      item_dyopt = "dyopt"
    )

  }

)
