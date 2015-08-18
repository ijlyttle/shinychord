library("shiny")
library("shinychord")

shinyUI(
  fluidPage(
    title = "",
    sidebarLayout(
      sidebarPanel(
        chord_tbl_1$ui_controller,
        chord_tbl_2$ui_controller
      ),
      mainPanel(
        chord_tbl_1$ui_view,
        chord_tbl_2$ui_view
      )
    )
  )
)
