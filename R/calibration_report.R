#' Generate a calibration report
#'
#' @description Use \code{rmarkdown::render()} to render a calibration report from
#' a calibration sequence .xls file.
#'
#' @param filepath Full path to a calibration .xls file.
#' @param output_folder Full path to the folder in which to render the report PDF.
#' @param report_filename NULL to copy the .xls filename, or a string to use in its place.
#'
#' @return Nothing in the workspace; a PDF will be rendered to the output_folder.
#'
#' @author Matt Harris
#'
#' @examples
#' calibration_report(list.files(directory, full.names = TRUE)[1], output_folder = "path/to/output/folder/")
#'
#' @importFrom rmarkdown render
#'
#' @export
#'
calibration_report <- function(filepath,
                               output_folder,
                               report_filename = NULL){
  ## test vars
  # filepath = cal_target
  # output_folder = report_dir
  # report_filename = NULL
  ## Find the .Rmd file
  # report_rmd <- system.file("rmd", "calibration-report.Rmd", package = "ICQC")
  # report_rmd <- paste0(here::here(),"/report-templates/calibration-report.Rmd")
  ## Generate filename
  if(is.null(report_filename)){
    report_filename <- paste0(strsplit(trim_path_int(filepath),"[.]")[[1]][1],"_Report")
  }
  render(input = system.file("rmd", "calibration-report.Rmd", package = "ICQC"),
         output_file = report_filename,
         output_dir = output_folder,
         params = list(cal_target = filepath, unts = "sec"),
         envir = new.env())
}
