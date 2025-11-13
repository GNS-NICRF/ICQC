#' Parse a sequence excel file.
#'
#' @description Read in and parse a sequence excel file into a nested list of its 
#' components. The sequence type (Calibration or sample run) is detected automatically
#' based on the filename. Ion type (Anion or Cation) must be specified.
#'
#' Ref2
#' @param filepath The full filepath to a sequence .xls file
#' @param ion_type One of either 'Anion' or 'Cation'.
#' @param verbose TRUE/FALSE to print status messages.
#' #'
#' @return A list containing sequence information and data. 
#'
#' @author Matt Harris
#'
#' @examples
#' ## Code examples here
#' sequence <- parse_sequence(filepath = "Drive:/full/path/to/file.xls", ion_type = "Anion")
#'                            
#' @importFrom magrittr %>%
#' 
#' @export
#' 
parse_sequence <- function(filepath,
                           ion_type,
                           verbose = TRUE){
  ## testvars
  # filepath <- list.files(data_dir, full.names = T)[1]
  # ion_type = "Cation"
  # verbose = TRUE
  ## To add
  # - check row/col 1/1 against filename to ensure the instrument field is consistent
  ## var pass
  type = ion_type
  ## Extension, for future use if file export format is ever updated from .xls
  # extn <- unlist(lapply(strsplit(trim_path_int(filepath),"[.]"),"[[",2))
  ## Determine if it is a calibration or sample run and parse accordingly
  if(grepl("Calibration",trim_path_int(filepath))){
    if(verbose){message("Parsing calibration sequence")}
    # Setup list. function wrap this perhaps
    cal_list <- vector("list", length = 9) %>% 'names<-'(c("Instrument","Sequence_Type","Ion_Type","Number","Date","DateYYYYMMDD","Data","Cal_Levels","Cal_Curves"))
    # File components
    filename_bits <- unlist(strsplit(trim_path_int(filepath),"[_.]"))
    # A couple of assumptions in these assignments. Instrument name assumes any matching of the character "BLIZ"
    # will only capture e.g., BLIZ and BLIZZARD, used for instrument names.
    # Pre-Blizzard (new 2025 ICs) instrument names don't have this and are just lumped in as 'Legacy IC'
    if(grep("BLIZ",filename_bits) != 0){
      cal_list$Instrument <- paste0(filename_bits[grep("BLIZ",filename_bits)],"_",filename_bits[grep("BLIZ",filename_bits)+1])
    } else {
      cal_list$Instrument <- "Legacy IC"
    }
    # Type matches the type var. Forced use of filename case.
    cal_list$Sequence_Type <- "Calibration"
    cal_list$Ion_Type <- filename_bits[grep(type,filename_bits, ignore.case = T)]
    cal_list$Number <- filename_bits[grep(type,filename_bits, ignore.case = T)+1] %>% as.numeric()
    cal_list$Date <- format(as.Date(filename_bits[1], "%Y%m%d"),"%d/%m/%Y")
    cal_list$DateYYYYMMDD <- filename_bits[1]
    ## Get the summary sheet data
    cal_list$Data <- parse_seq_summary(filepath, type)
    ## Calibration levels 
    cal_list$Cal_Levels <- parse_cal_levels(filepath,type)
    ## Curve information
    cal_list$Cal_Curves <- parse_cal_curve_info(filepath)
    # Naming scheme for return
    return_list <- cal_list
  } else {
    if(verbose){message("Parsing sample sequence")}
    ## Code to come
  }
  return(return_list)
}
