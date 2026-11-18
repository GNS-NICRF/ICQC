#' Extract and generated version ID from an analytical sequence filepath for display in rendered pdf
#'
#' @description Formats a label for printing in the YAML header of calibration-report.Rmd
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
ana_version_for_YAML <- function(filename){
  file_splits <- unlist(strsplit(trim_path_int(filename),"[_.]"))
  gen_id <- str_remove(paste(file_splits[seq(which(!is.na(str_extract(file_splits, "\\d+")))[4],length(file_splits),1)],collapse = "_"), "_xls")
  ret_label = paste0("chromeleon export ",gen_id)
  ret_label
}
