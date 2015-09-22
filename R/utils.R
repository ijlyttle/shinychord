# df_set_tz
#
# Change the timezone of all the POSIX.ct object to tz
#
# @param df   dataframe
# @param tz   Olson timezone
#
# @return dataframe
# @export
#
# df_set_tz <- function(df, tz = "UTC"){
#
#   fn_tz <- function(x){
#
#     if (!lubridate::is.POSIXct(x)) return(x) # do nothing
#
#     lubridate::with_tz(x, tz)
#   }
#
#   dplyr::mutate_each_(df, funs(fn_tz), colnames(df))
# }

#' rctval_names
#'
#' @param rctval
#'
#' @return character vector
#' @export
#'
rctval_names <- function(rctval){

  lst <- shiny::reactiveValuesToList(rctval)

  is_null <- vapply(lst, is.null, FALSE)

  names(lst[!is_null])

}

#' df_names_inherits
#'
#' get the names of all the columns of the dataframe
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
