#' Closure for file-downloading
#'
#' A closure to provide a function to download a file,
#' by copying it from the specified place. Useful for
#' \code{shiny::downloadHandler()}.
#'
#' @param file_source  character describing path to file to be copied
#'
#' @return function
#' @export
#
cl_file_copy <- function(file_source){

  function(file){
    file.copy(from = file_source, to = file)
  }
}



#' Get the names of all the columns of the dataframe
#' that inherit from the supplied class name
#'
#' @param df     dataframe
#' @param what   character vector of class we wish to find
#'
#' @return character vector
#' @export
#
df_names_inherits <- function(df, what){

  inherits_class <- vapply(df, inherits, logical(1), what = what)

  names_class <- names(inherits_class)[inherits_class]

  names_class
}


#' Sets the timezone of all time-based columns in a dataframe
#'
#' @param df  dataframe
#' @param tz  timezone, an Olson timezone or "UTC" (default)
#'
#' @return dataframe
#'
#' @examples
#' df_with_tz(coltypes_sample, tz = "America/Chicago")
#'
#' @export
#
df_with_tz <- function(df, tz = "UTC"){

  is_datetime <- vapply(df, inherits, logical(1), "POSIXct")

  fn_set_tz <- function(x){
    attr(x, "tzone") <- tz
    x
  }

  df[is_datetime] <- lapply(df[is_datetime], fn_set_tz)

  df
}

#

#' function for scrollable pre-formatted text
#'
#' This is used as the \code{container} argument in \code{shiny::htmlOutput}
#'
#' @param ... expression used to fill text
#'
#' @source \url{http://stackoverflow.com/questions/10374171/how-to-make-twitter-bootstraps-pre-blocks-scroll-horizontally}
#' @export
#
pre_scroll <- function(...){
  htmltools::pre(
    ...,
    style = "overflow: auto; word-wrap: normal; white-space: pre;"
  )
}
