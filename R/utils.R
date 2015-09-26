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
#'
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
#'
df_names_inherits <- function(df, what){

  inherits_class <- vapply(df, inherits, logical(1), what = what)

  names_class <- names(inherits_class)[inherits_class]

  names_class
}
