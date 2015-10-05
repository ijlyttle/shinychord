
shinyUI(
  fluidPage(
    titlePanel("dygraphs demo"),
    sidebarLayout(
      # generally, controllers go in a sidebarPanel
      sidebarPanel(
        chord_dygraph$ui_controller
      ),
      # generally, views go in the mainPanel
      mainPanel(
        chord_dygraph$ui_view
      )
    )
  )
)
