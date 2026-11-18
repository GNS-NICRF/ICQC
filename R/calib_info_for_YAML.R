#' Format a title label from calibration filepath
#'
#' @description Formats a label for printing in the YAML header of calibration-report.Rmd
#'
#' @author Matt Harris
#'
#' @param filename full filename of the calibration sequence .xls file.
#'
#' @return A character string
#'
#' @noRd
#'
calib_info_forYAML <- function(filename){
  file_splits <- strsplit(trim_path_int(filename),"_|\\.")[[1]]
  ret_label = paste0(file_splits[1]," ",file_splits[2]," ",file_splits[3],": ",file_splits[5]," ",file_splits[6]," & ",file_splits[7]," ",file_splits[8])
  ret_label
}
