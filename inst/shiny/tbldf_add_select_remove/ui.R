library("shiny")
library("shinyjs")
library("shinychord")

shinyUI(
  fluidPage(
    useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        h4("CSV Import"),
        chord_tbl$ui_controller,
        h4("Add"),
        chord_list_tbl_add$ui_controller,
        h4("Select"),
        chord_list_tbl_select$ui_controller,
        h4("Remove"),
        chord_list_tbl_remove$ui_controller
      ),
      mainPanel(
        h4("CSV Import"),
        chord_tbl$ui_view,
        h4("Add"),
        chord_list_tbl_add$ui_view,
        h4("Select"),
        chord_list_tbl_select$ui_view,
        h4("Remove"),
        chord_list_tbl_remove$ui_view
      )
    )
  )
)
