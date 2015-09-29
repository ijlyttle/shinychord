library("shiny")
library("shinychord")

shinyApp(

  ui = fluidPage(
    titlePanel("shell"),
    sidebarLayout(
      sidebarPanel(),
      mainPanel()
    )
  ),

  server = function(input, output, session){

  }

)
