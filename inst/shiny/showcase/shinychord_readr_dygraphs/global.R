library("shiny")
library("shinychord")
library("shinyjs")
library("shinyBS")
library("dplyr")

# Shinychords are created in global.R, so that the members of the
# shinychord list are created "together", and are available to
# ui.R and shiny.R.
#
# The id argument is used to help ensure uniqueness among the id's
# of the shiny inputs and outputs.
#
chord_readr <- ch_read_delim(id = "readr")
chord_dygraph <- ch_dygraph(id = "dyg")
