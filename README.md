# shinychord

The purpose of this package is to propose a convention for reusable shiny modules.

This might be useful for shiny code where:

- things can be broken down into modules
- within each module, the are a lot of controls are important only within the module
- there may be a need to use modules in different apps

One benefit is that the server logic for a given module is encapsulated, while exposing only a few reactive values and the ui elements.

## tl;dr

- [PDF of a presentation](https://ijlyttle.github.io/shinychord/doc/isu_presentation.pdf) made to the ISU Statistics Graphics Group - 2015-10-01. Revised 2015-10-05.

- [Demonstration app](https://ijlyttle.shinyapps.io/shinychord_dygraphs), showing a [dygraph](https://rstudio.github.io/dygraphs/) shinychord in a shiny-app.

- [Demonstration app](https://ijlyttle.shinyapps.io/shinychord_readr_dygraphs), showing a [csv-parser](https://github.com/hadley/readr) shinychord and a [dygraph](https://rstudio.github.io/dygraphs/) shinychord in a shiny app.

## Structure of a shinychord

A shinychord is simply a function that takes `id` as its argument. It returns a list with three items:

- `ui_controller` a `shiny::tagList` containing ui elements focusing on inputs
- `ui_view` a `shiny::tagList` containing ui elements focusing on outputs
- `server_model` a function that contains all the server logic, and exposes reactive values

As you might imagine, the names on this list are inspired by the model-view-controller paradigm.

There is nothing mandatory or scared about the names of these elements, it is only a proposed convention. Furthermore, the elements of each `shiny::taglist` are available if you want to select or arrange them differently.

## Example

Let's say we wanted a shinychord to upload and parse a delimited file. We start with a template for a shinychord:

```R
ch_upload_parse <- function(id){

  # controller
  ui_controller <- shiny::tagList()

  # view
  ui_view <- shiny::tagList()

  # model
  server_model <- function(
    input, output, session
  ){
  
  }
  
  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )
}
```

Over the next few sections, we show how the parts of the template are filled out so that we end with a complete shinychord.

### Naming function

One of the challenges we have in composing shiny applications is a way to keep the names unique among each of the elements.

To help us, I put a function at the top of the shinychord:

```R
id_name <- function(...){
  paste(list(id, ...), collapse = "_")
}
```

This function will be very handy as we define the inputs and outputs.

### Controller

We could make this as fancy as we like, but let's consider a simple set of controls, comprised of a file-upload input and a select input (to choose a delimiter)

```R
ui_controller <- shiny::tagList()

id_controller_file <- id_name("controller", "file")
ui_controller$file <-
  shiny::fileInput(
    inputId = id_controller_file,
    label = "File",
    accept = c("text/csv", ".csv", "text/comma-separated-values", "text/plain")
  )
```

You can see that we use the `id_name()` function to combine the `id` string (supplied as an
argument to the shiny chord) with the local identifier.

We also want an input to choose the delimiter:

```R
id_controller_delim <- id_name("controller", "delim")
ui_controller$delim <-
  shiny::selectizeInput(
    inputId = id_controller_delim,
    label = "Delimiter",
    choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
    selected = ";"
  )
```

### View

We will want a couple of view elements - one to preview the text that has been uploaded, and another to preview the parsed dataframe.

```R
ui_view <- shiny::tagList()

# shows the raw text of the file (first few lines)
id_view_text <- id_name("view", "text")
ui_view$text <- shiny::verbatimTextOutput(id_view_text)

# shows a glimpse of the parsed data-frame
id_view_data <- id_name("view", "data")
ui_view$data <- shiny::verbatimTextOutput(id_view_data)
```

### Model

A couple of notes here:

- Whenever we refer to a reactive value, we have to wrap it in a reactive expression.

- For this shinychord, the ultimate goal is to encapsulate the details so that we expose an `id`, when we create the shinychord, and a reactive value, when we place the `server_model()` function in the shiny `server()` function. We have to pass to this `server_model()` function the reactive value and the name of the item within the reactive value. It will be in this reactive value that the parsed dataframe will be placed.

```R
server_model <- function(
  input, output, session,
  rctval_data, item_data
){

  ## reactives

  # reactive to read in the raw text from the file-specification input
  rct_txt <- reactive({

    shiny::validate(
      shiny::need(env$input[[id_controller_file]], "File not selected")
    )

    infile <- input[[id_controller_file]]$datapath

    readr::read_file(infile)
  })

  ## observers

  # this needs to be wrapped in a reactive expression
  observe({
    rctval_data[[item_data]] <-
      readr::read_delim(
        file = rct_txt(),
        delim = input[[id_controller_delim]]
      )
  })

  ## outputs

  # sets the output for the raw text
  output[[id_view_text]] <-
    renderText({
      
      shiny::validate(
        shiny::need(rct_txt(), "File did not load properly")
      )
      
      h <- rct_txt()
      h <- readr::read_lines(h, n_max = 10)
      
      paste(h, collapse = "\n")
    })


  # sets the output for the parsed dataframe
  output[[id_view_data]] <-
    renderPrint({
      
      shiny::validate(
        shiny::need(rctval_data[[item_data]], "No data")
      )
      
      dplyr::glimpse(rctval_data[[item_data]])
    })

}
```

## Completed shinychord

So here are all the elements from above, combined into a shinychord function:

```R
ch_upload_parse <- function(id){

  id_name <- function(...){
    paste(list(id, ...), collapse = "_")
  }

  # controller
  ui_controller <- shiny::tagList()

  id_controller_file <- id_name("controller", "file")
  ui_controller$file <-
    shiny::fileInput(
      inputId = id_controller_file,
      label = "File",
      accept = c("text/csv", ".csv", "text/comma-separated-values", "text/plain")
    )

  id_controller_delim <- id_name("controller", "delim")
  ui_controller$delim <-
    shiny::selectizeInput(
      inputId = id_controller_delim,
      label = "Delimiter",
      choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
      selected = ";"
    )
  
  # view
  ui_view <- shiny::tagList()
  
  # shows the raw text of the file (first few lines)
  id_view_text <- id_name("view", "text")
  ui_view$text <- shiny::verbatimTextOutput(id_view_text)
  
  # shows a glimpse of the parsed data-frame
  id_view_data <- id_name("view", "data")
  ui_view$data <- shiny::verbatimTextOutput(id_view_data)
  
  # model
server_model <- function(
  input, output, session,
  rctval_data, item_data
){

  ## reactives

  # reactive to read in the raw text from the file-specification input
  rct_txt <- reactive({

    shiny::validate(
      shiny::need(input[[id_controller_file]], "File not selected")
    )

    infile <- input[[id_controller_file]]$datapath

    readr::read_file(infile)
  })

  ## observers

  # this needs to be wrapped in a reactive expression
  observe({
    rctval_data[[item_data]] <-
      readr::read_delim(
        file = rct_txt(),
        delim = input[[id_controller_delim]]
      )
  })

  ## outputs

  # sets the output for the raw text
  output[[id_view_text]] <-
    renderText({
      
      shiny::validate(
        shiny::need(rct_txt(), "File did not load properly")
      )
      
      h <- rct_txt()
      h <- readr::read_lines(h, n_max = 10)
      
      paste(h, collapse = "\n")
    })


  # sets the output for the parsed dataframe
  output[[id_view_data]] <-
    renderPrint({
      
      shiny::validate(
        shiny::need(rctval_data[[item_data]], "No data")
      )
      
      dplyr::glimpse(rctval_data[[item_data]])
    })

}  
  list(
    ui_controller = ui_controller,
    ui_view = ui_view,
    server_model = server_model
  )
}
```

### Using the shinychord in an app

Here's where we can see a benefit of this approach, as all the interal complexity of the shinychord has been encapuslated, exposing only the bits we need.

For the shiny developer, instead of developing a file-parser for every occasion, or cutting and pasting from a template (and managing the id's), she or he can use a shinychord - set an `id` in one place, and create a reactive value into which the `server_model` function can leave the resulting dataframe.

The shinychord function can be created once and used everywhere.

```R
library("shiny")
library("readr")
library("dplyr")

chord_parse <- ch_upload_parse("parse")

shinyApp(

  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(chord_parse$ui_controller),
      mainPanel(chord_parse$ui_view)
    )
  ),
  
  server = function(input, output, session){
  
    rctval <- reactiveValues(data_csv = NULL)
    
    chord_parse$server_model(
      input, output, session,
      rctval_data = rctval, item_data = "data_csv"
    )
    
    observe(print(rctval$data_csv))
    
  }
  
)
```
