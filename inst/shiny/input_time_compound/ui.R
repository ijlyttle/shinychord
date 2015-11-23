shinyUI(
  tagList(
    includeCSS(system.file(file.path("css", "shinychord.css"), package = "shinychord")),
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
)
