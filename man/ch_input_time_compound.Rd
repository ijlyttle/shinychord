% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch_input_time_compuound.R
\name{ch_input_time_compound}
\alias{ch_input_time_compound}
\title{Creates a collection of shiny objects to wrap a compound time-input.}
\usage{
ch_input_time_compound(id, label = "", step = c(1, 5), default = c(1, 30))
}
\arguments{
\item{id}{character, tag to prepend to the input and output id's}

\item{label}{character, label for the compound input}

\item{step}{numeric vector for the steps, (hours, minutes)}

\item{default}{numeric lubridate duration for the default}
}
\value{
list containing \code{ui_controller}, \code{ui_view}, and \code{server_model}
}
\description{
The list will contain:
}
\details{
\describe{
  \item{\code{ui_controller}}{\code{shiny::taglist} of ui elements for the controller}
  \item{\code{ui_view}}{\code{shing::taglist} of ui elements for the view}
  \item{\code{server_model}}{function with reactive code}
}

The list returned by this function has to be available to both the ui and the server.
If not using the \code{shinyApp} formulation, perhaps \code{global.R} could be useful.

The list \code{ui_controller} will have inputs:

\describe{
 \item{\code{hours}}{\code{shiny::numericInput} used to set the number of hours}
 \item{\code{minutes}}{\code{shiny::numericInput} used to set the number of minutes}
}

The list \code{ui_view} will have members:

\describe{
 \item{\code{view_text}}{\code{shiny::textOutput} displays vector of hours and minutes}
}

The function \code{server_model()} will be called from your server function.
Its arguments are:

\describe{
 \item{\code{input, output, session}}{input, output, session values passed from your server function}
 \item{\code{rctval_input, item_input}}{
   \code{shiny::reactiveValues} object, character string.
   \code{rctval_input[[item_input]]} lubridate duration, output - number of seconds
 }
}
}

