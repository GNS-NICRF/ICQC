# Utility functions

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

#' Trim a filepath
#'
#' @description Isolate a file or folder name from a file path. Assumes windows
#' file tree conventions (i.e., forward slash "/")
#'
#' @author Matt Harris
#'
#' @param filenames one or more paths to an object.
#'
#' @return A vector containing the supplied filenames, with the paths removed.
#'
#' @noRd
#'
trim_path_int <- function(filenames){
  if(length(filenames) > 1){
    it_list <- vector(mode = "list", length = length(filenames))
    trimmed_filenames <- vector(mode = "character", length = length(filenames))
    for(f in seq_along(filenames)){
      filename_trimmed <- unlist(strsplit(filenames[f],"/"))[length(unlist(strsplit(filenames[f],"/")))]
      trimmed_filenames[f] <- filename_trimmed
    }
    trimmed_filenames
  } else if(length(filenames) == 1){
    filename <- filenames
    filename_trimmed <- unlist(strsplit(filename,"/"))[length(unlist(strsplit(filename,"/")))]
    filename_trimmed
  } else{
    message("Empty object; no path to trim. Filenames may be missing.")
  }
}
