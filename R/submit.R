#' @import methods
#' @import data.table
#' @import stringr
#' @import whisker
#' @import yaml
NULL


#' Get the file name from a full path.
#'
#' Such as the file name of folder/file.txt is file.txt.
#'
#' @param path character a file path which can be a full path or not.
#' @return character the file name of the given full path
#' @export
get_name_from_path <- function(path) {
  stopifnot(length(path) == 1)
  name <- str_split(path, "/")[[1]]
  return(name[length(name)])
}
