library("shiny")
library("shinychord")
library("shinyjs")
library("shinyBS")
library("dplyr")

chord_readr <- ch_read_delim(id = "readr")
chord_dygraph <- ch_dygraph(id = "dyg")

shinyApp(

  ui = fluidPage(
    useShinyjs(),
    titlePanel("readr & dygraph demo"),
    sidebarLayout(
      sidebarPanel(
        chord_readr$ui_controller,
        chord_dygraph$ui_controller
      ),
      mainPanel(
        chord_readr$ui_view,
        chord_dygraph$ui_view
      )
    )
  ),

  server = function(input, output, session){

    rctval <- reactiveValues(
      data = NULL,
      dyopt = list(useDataTimezone = TRUE)
    )

    chord_readr$server_model(
      input, output, session,
      rctval_data = rctval, item_data = "data"
    )

    chord_dygraph$server_model(
      input, output, session,
      rctval_data = rctval, item_data = "data",
      rctval_dyopt= rctval, item_dyopt = "dyopt"
    )
  }
)
