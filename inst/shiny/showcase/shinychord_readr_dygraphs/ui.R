
shinyUI(
  fluidPage(
    useShinyjs(),
    titlePanel("readr & dygraphs demo"),
    sidebarLayout(
      # generally, controllers go in a sidebarPanel
      sidebarPanel(
        chord_readr$ui_controller,
        chord_dygraph$ui_controller
      ),
      # generally, views go in the mainPanel
      mainPanel(
        chord_readr$ui_view,
        chord_dygraph$ui_view
      )
    )
  )
)
