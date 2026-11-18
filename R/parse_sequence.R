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
#' @param matching_cal_dir NULL or full path to directory containing calibration files in which to search for one matching the QCs in a target sample sequence. Only needed if the target sequence to parse is a sample/analytical sequence.
#'
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
                           verbose = TRUE,
                           matching_cal_dir = NULL){
  ## testvars
  # filepath <- list.files(data_dir_raw_3, full.names = T)[1]
  # ion_type = "Cation"
  # verbose = TRUE
  # matching_cal_dir = data_dir
  ## var pass
  type = ion_type
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
    if(verbose){message("Parsing sample/analytical sequence")}
    # Input data list
    data_list <- vector("list", length = 10) %>% 'names<-'(c("Instrument","Sequence_Type","Ion_Type","Core","Sample_Range","Date","DateYYYYMMDD","Export_Version","Data","Matching_Calibration"))
    filename_bits <- unlist(strsplit(trim_path_int(filepath),"[_.]"))
    # Bits and pieces
    if(grep("BLIZ",filename_bits) != 0){
      data_list$Instrument <- paste0(filename_bits[grep("BLIZ",filename_bits)],"_",filename_bits[grep("BLIZ",filename_bits)+1])
    } else {
      data_list$Instrument <- "Legacy IC"
    }
    # Type matches the type var. Forced use of filename case.
    data_list$Sequence_Type <- "Analytical"
    data_list$Ion_Type <- ion_type
    # Core is a bit tricky - this code pulls out and appends the bits between the end of the instrument string (BLIZ_NAME) and
    # the first number after the date in the filename string
    data_list$Core <- paste(filename_bits[seq(grep("BLIZ",filename_bits)+2,which(!is.na(str_extract(filename_bits, "\\d+")))[2]-1,1)], collapse = "_")
    data_list$Sample_Range <- paste(filename_bits[which(!is.na(str_extract(filename_bits, "\\d+")))[2]],"-",filename_bits[which(!is.na(str_extract(filename_bits, "\\d+")))[3]])
    # cal_list$Number <- filename_bits[grep(type,filename_bits, ignore.case = T)+1] %>% as.numeric()
    data_list$Date <- format(as.Date(filename_bits[1], "%Y%m%d"),"%d/%m/%Y")
    data_list$DateYYYYMMDD <- filename_bits[1]
    data_list$Export_Version <- paste(filename_bits[seq(which(!is.na(str_extract(filename_bits, "\\d+")))[2],length(filename_bits),1)],collapse = "_")
    data_list$Data <- parse_seq_summary(filepath, type)
    ## Identify matching calibration. Currently a fail condition if QCs in a run map to different calibrations.
    matching_QCs <- data_list$Data$Calibration[grep(type,data_list$Data$Name)]
    if(!all(matching_QCs == matching_QCs[1])){stop(type," QCs in run ",trim_path_int(filepath)," are recorded as being from multiple calibrations (",paste(unique(matching_QCs),collapse = ", "),"). This is currently a fatal exception for the processing code.")}
    matching_cal <- unique(matching_QCs)
    # Get the data
    if(!is.null(matching_cal_dir)){
      files <- list.files(matching_cal_dir, recursive = T, full.names = T)
      matching_cal_file <- files[grep(paste0("_",matching_cal,"_"), trim_path_int(files))]
      if(length(matching_cal_file) == 0){
        stop("Can't find a calibration file matching the QCs in ",trim_path_int(filepath),". Target: ",type," ",matching_cal)
      }
      # Load in matching calibration file
      data_list$Matching_Calibration <- parse_sequence(matching_cal_file, ion_type = ion_type, verbose = F)
      if(verbose){message("Matching calibration file ", trim_path_int(matching_cal_file)," appended to analytical run data list")}
    } else {
      if(verbose){message("No matching_cal_dir supplied; calibration will not be attached to analytical run data.")}
      data_list$Matching_Calibration = NULL
    }
    return_list <- data_list
  }
  return(return_list)
}
