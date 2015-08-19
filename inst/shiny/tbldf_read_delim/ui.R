library("shiny")
library("shinyjs")
library("shinychord")

shinyUI(
  fluidPage(
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        h3("Set 1"),
        chord_tbl_1$ui_controller,
        chord_list_tbl_1_add$ui_controller,
        chord_list_tbl_1_remove$ui_controller,
        h3("Set 2"),
        chord_tbl_2$ui_controller
      ),
      mainPanel(
        h3("Set 1"),
        chord_tbl_1$ui_view,
        chord_list_tbl_1_add$ui_view,
        chord_list_tbl_1_remove$ui_view,
        h3("Set 2"),
        chord_tbl_2$ui_view
      )
    )
  )
)
