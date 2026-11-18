#' Format a title label from analytical sequence filepath
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
ana_info_for_YAML <- function(filename){
  file_splits <- unlist(strsplit(trim_path_int(filename),"[_.]"))
  # Structure should be yyyymmdd INSTR NAME: CORE NAME SAMPLE-SAMPLE
  date <- file_splits[1]
  instrument <- paste0(file_splits[grep("BLIZ",file_splits)],"_",file_splits[grep("BLIZ",file_splits)+1])
  core <-  paste(file_splits[seq(grep("BLIZ",file_splits)+2,which(!is.na(str_extract(file_splits, "\\d+")))[2]-1,1)], collapse = "_")
  samples <- paste(file_splits[which(!is.na(str_extract(file_splits, "\\d+")))[2]],"-",file_splits[which(!is.na(str_extract(file_splits, "\\d+")))[3]])
  gen_id <- str_remove(paste(file_splits[seq(which(!is.na(str_extract(file_splits, "\\d+")))[4],length(file_splits),1)],collapse = "_"), "_xls")
  ret_label = paste0(date," ",instrument,", ",core," ",samples)
  ret_label
}
