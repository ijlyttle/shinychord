library("shiny")
library("shinychord")

shinyUI(
  fluidPage(
    title = "",
    sidebarLayout(
      sidebarPanel(
        chord_tbl_1$ui_control,
        chord_tbl_2$ui_control
      ),
      mainPanel(
        chord_tbl_1$ui_response,
        chord_tbl_2$ui_response
      )
    )
  )
)
