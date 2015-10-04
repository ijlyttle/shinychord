
shinyUI(
  fluidPage(
    titlePanel("dygraphs demo"),
    sidebarLayout(
      sidebarPanel(chord_dygraph$ui_controller),
      mainPanel(chord_dygraph$ui_view)
    )
  )
)
