


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
