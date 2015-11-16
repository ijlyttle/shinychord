shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h3("Enter in time"),
        chord_read_time$ui_controller
      ),
      mainPanel(
        h3("View time"),
        chord_read_time$ui_view
      )
    )
  )
)
