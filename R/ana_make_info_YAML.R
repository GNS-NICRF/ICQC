#' Format some info for YAML author line from an analytical sequence filepath
#'
#' @description Formats a label for printing in the YAML header of analytical-sequence.Rmd. Gets around the pesky linebreak issue.
#'
#' @author Matt Harris
#'
#' @param filename full filename of the analytical sequence .xls file.
#'
#' @return A character string
#'
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#'
#' @noRd
#'
ana_make_info_YAML <- function(filename){
  paste(
    ana_info_for_YAML(filename),
    ana_version_for_YAML(filename),
    sep = "\\\\"
  )
}
