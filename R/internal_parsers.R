# Parsing functions that are not exported.

#' Parse sequence summary
#'
#' @description Parse the summary page of a sequence .xls file into a data.frame
#'
#' @param filepath Full filepath to .xls file containing ion summary sheet.
#' @param ion_type One of either 'Anion' or 'Cation' to target the respective summary sheet.
#' @param xls_type One of either "Detect", "Calibration" or "Analytical" to denote the type of sequence this summary is from. If set to "Detect", the filename will be used to evaluate the run type.
#' @param header_lines_ind row indices of header lines in data. Defaults to first four.
#' @param verbose TRUE/FALSE to show messages during \code{readxl::read_xls()} call.
#'
#' @return Formatted data frame containing sequence summary data.
#'
#' @author Matt Harris
#'
#' @examples
#' summary_data <- parse_seq_summary(filepath, type)
#'
#' @importFrom readxl read_xls
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom utils tail
#'
#' @noRd
parse_seq_summary <- function(filepath,
                              ion_type,
                              xls_type = 'Detect',
                              header_lines_ind = seq(1,4,1),
                              verbose = FALSE){
  ## Test vars
  # filepath = filepath
  # ion_type = "Cation"
  # xls_type = "Analytical"
  # header_lines_ind = seq(1,4,1)
  # verbose = T
  # Auto xls type detection
  # Pretty simple - if it doesn't have "Calibration" in the file name, then it's an analytical sequence.
  if(xls_type == 'Detect'){
    if(grepl("Calibration",trim_path_int(filepath))){
      xls_type = 'Calibration'
    } else {
      xls_type = 'Analytical'
    }
  }
  # Var pass
  type = ion_type
  # Read in file
  if(verbose){
    rawsheet <- read_xls(path = filepath,
                         sheet = grep(type,readxl::excel_sheets(filepath)),
                         range = cell_limits(c(1, NA), c(NA,NA)),
                         na = c("n.a.",NA,""), col_names = F, skip = 0)
  } else {
    suppressMessages(rawsheet <- read_xls(path = filepath,
                                          sheet = grep(type,readxl::excel_sheets(filepath)),
                                          range = cell_limits(c(1, NA), c(NA,NA)),
                                          na = c("n.a.",NA,""), col_names = F, skip = 0))
  }
  # Headers, dupe testing
  header_lines_comb <- apply(rawsheet[header_lines_ind,], 2, function(col) paste(col, collapse = "_")) %>% 'names<-'(c(NULL))
  dupes <- which(duplicated(header_lines_comb)) %>% as.numeric()
  # Remove duplicates
  rawsheet <- rawsheet %>% select(-all_of(dupes))
  if(length(dupes) != 0){header_lines_comb = header_lines_comb[-c(dupes)]}
  # Starter columns: those before the first 'amount' column
  # Is this a sample export, where calibration is tracked as a separate column?
  if(xls_type == "Analytical" | xls_type == "Sample"){
    starter_columns <- rawsheet[seq(max(header_lines_ind)+1,nrow(rawsheet),1),seq(1,c(grep("Amount",header_lines_comb)[1])-1,1)] %>%
      'colnames<-'(c("Injection_Number","Type","Level","Dilution","QC_Version_Number","Calibration","Inject_Time","Name")) %>%
      mutate(Level = as.numeric(str_extract(Level, "\\d+"))) %>%
      mutate(across(c(Injection_Number,Dilution,Calibration), ~as.numeric(.))) %>%
      mutate(Inject_Time = as.POSIXct(as.numeric(Inject_Time) * 86400, origin = "1899-12-30", tz = "UTC"))
  } else if(xls_type == "Calibration"){
    starter_columns <- rawsheet[seq(max(header_lines_ind)+1,nrow(rawsheet),1),seq(1,c(grep("Amount",header_lines_comb)[1])-1,1)] %>%
      'colnames<-'(c("Injection_Number","Type","Level","Dilution","QC_Type","QC_Version","Inject_Time","Name")) %>%
      mutate(across(c(Injection_Number,Level,Dilution,QC_Version), ~as.numeric(.))) %>%
      mutate(Inject_Time = as.POSIXct(as.numeric(Inject_Time) * 86400, origin = "1899-12-30", tz = "UTC"))
  } else {
    stop("xls_type must be one of either 'Calibration' or 'Analytical'.")
  }
  # Data headers: those after (+ including) the first 'amount' column
  data_headers <- header_lines_comb[c(grep("Amount",header_lines_comb)[1]):ncol(rawsheet)]
  data_headers_re <- paste0(
    unlist(lapply(strsplit(data_headers,"[_]"),tail, n = 1L)),"_",
    unlist(lapply(strsplit(data_headers,"[_]"),"[[",1)))
  data_columns <- rawsheet[seq(max(header_lines_ind)+1,nrow(rawsheet),1),
                           seq(ncol(starter_columns)+1,ncol(rawsheet),1)] %>%
    'colnames<-'(c(data_headers_re))
  # bind
  data_re <- cbind(starter_columns,data_columns)
  # remove non Type lines
  if(xls_type == "Calibration"){
    data_re_type <- data_re[grep(type,data_re$Name),] # For calibrations, we dispense with waters and other injections.
    return(data_re_type)
  } else {
    return(data_re)
  }
}


#' Parse calibration levels sheet
#'
#' @description Parse the calibration levels page of a sequence .xls file into a data.frame
#'
#' @param filepath Full filepath to .xls file containing calibration levels sheet
#' @param ion_type One of either 'Anion' or 'Cation' to target the respective rows.
#' Shouldn't matter too much as the information is generally duplicated across each.
#' @param header_lines_ind row indices of header lines in data. Defaults to first four.
#' @param verbose TRUE/FALSE to show messages during \code{readxl::read_xls()} call.
#' @param drop_only_NA TRUE/FALSE to drop columns containing only NA. This is
#' intended to drop trailing columns that are sometimes included that by
#' \code{readxl::read_xls()} that contain only whitespace.
#'
#' @return Formatted data frame containing calibration levels data.
#'
#' @author Matt Harris
#'
#' @examples
#' summary_data <- parse_cal_levels(filepath, type)
#'
#' @importFrom readxl read_xls
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom utils tail
#'
#' @noRd
parse_cal_levels <- function(filepath,
                             ion_type,
                             header_lines_ind = seq(1,4,1),
                             verbose = FALSE,
                             drop_only_NA = TRUE){
  ## Test vars
  # filepath = filepath
  # ion_type = type
  # header_lines_ind = seq(1,4,1)
  # verbose = F
  # drop_only_NA = TRUE

  # Var pass
  type = ion_type
  # Read in file
  if(verbose){
    rawsheet <- read_xls(path = filepath,
                         sheet = grep("Levels",readxl::excel_sheets(filepath)),
                         range = cell_limits(c(1, NA), c(NA,NA)),
                         na = c("n.a.",NA,""), col_names = F, skip = 0)
  } else {
    suppressMessages(
      rawsheet <- read_xls(path = filepath,
                           sheet = grep("Levels",readxl::excel_sheets(filepath)),
                           range = cell_limits(c(1, NA), c(NA,NA)),
                           na = c("n.a.",NA,""), col_names = F, skip = 0))
  }
  # Contd
  header_lines_comb <- apply(rawsheet[header_lines_ind,], 2, function(col) paste(col, collapse = "_")) %>% 'names<-'(c(NULL))
  # Starter columns: those before the first 'amount' column
  starter_columns <- rawsheet[seq(max(header_lines_ind)+1,nrow(rawsheet),1),seq(1,c(grep("Amount",header_lines_comb)[1])-1,1)] %>%
    'colnames<-'(c("Injection_Number","Injection_Name","Inject_Time","Position","Level")) %>%
    mutate(across(c(Injection_Number,Level), ~as.numeric(.)))
  # Data headers: those after (+ including) the first 'amount' column
  data_headers <- header_lines_comb[c(grep("Amount",header_lines_comb)[1]):ncol(rawsheet)]
  data_headers_re <- paste0(
    unlist(lapply(strsplit(data_headers,"[_]"),tail, n = 1L)),"_",
    unlist(lapply(strsplit(data_headers,"[_]"),"[[",1)))
  data_columns <- rawsheet[seq(max(header_lines_ind)+1,nrow(rawsheet),1),
                           seq(ncol(starter_columns)+1,ncol(rawsheet),1)] %>%
    'colnames<-'(c(data_headers_re))
  if(drop_only_NA){
    data_columns <- data_columns %>%
      select(where(~!all(is.na(.x))))
  }
  # bind
  data_re <- cbind(starter_columns,data_columns)
  # remove non Type lines
  data_re_type <- data_re[grep(type,data_re$Injection_Name),]
  return(data_re_type)
}


#' Parse calibration curve info sheet
#'
#' @description Parse the calibration curve info page of a sequence .xls file into a data.frame
#'
#' @param filepath Full filepath to .xls file containing calibration curve info sheet
#' @param header_lines_ind row indices of header lines in data. Defaults to first three.
#' @param verbose TRUE/FALSE to show messages during \code{readxl::read_xls()} call.
#'
#' @return Formatted data frame containing calibration curve info data.
#'
#' @author Matt Harris
#'
#' @examples
#' summary_data <- parse_cal_curve_info(filepath)
#'
#' @importFrom readxl read_xls
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @noRd
parse_cal_curve_info <- function(filepath,
                                 header_lines_ind = seq(1,3,1),
                                 verbose = FALSE){
  ## Test vars
  # filepath = filepath
  # header_lines_ind = seq(1,4,1)
  # verbose = F
  ## Read in file
  if(verbose){
    rawsheet <- read_xls(path = filepath,
                         sheet = grep("Curve",readxl::excel_sheets(filepath)),
                         range = cell_limits(c(1, NA), c(NA,NA)),
                         na = c("n.a.",NA,""), col_names = F, skip = 0)
  } else {
    suppressMessages(
      rawsheet <- read_xls(path = filepath,
                           sheet = grep("Curve",readxl::excel_sheets(filepath)),
                           range = cell_limits(c(1, NA), c(NA,NA)),
                           na = c("n.a.",NA,""), col_names = F, skip = 0))
  }
  # Contd
  header_lines_comb <- apply(rawsheet[header_lines_ind[1],], 2, function(col) paste(col, collapse = "_")) %>% 'names<-'(c(NULL))
  # Just all the columns.
  data <- rawsheet[c(header_lines_ind[3]+1:nrow(rawsheet)),] %>%
    filter(., rowSums(is.na(.)) != ncol(.)) %>%
    select(-c(1,2)) %>%
    'colnames<-'(c("Name","Ret_Time","Cal_Type","Cal_Fn","nPoints","R_Squared")) %>%
    as.data.frame() # Force data frame
  return(data)
}

#' Parse the formula of a calibration
#'
#' @description Return the calibration regression formula from a calibration list object
#' as a formula object to pass to e.g., \code{stats::lm()}
#'
#' @param parsed_calibration output of \code{parse_sequence()}, applied to a calibration sequence.
#' @param analyte_name character, name of analyte. E.g., "MSA".
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
parse_cal_formula <- function(parsed_calibration,
                              analyte_name){
  ## Test vars
  # parsed_calibration = test_Anion
  # analyte_name = "MSA"
  # Curve sheet
  curve_sheet = parsed_calibration$Cal_Curves
  # What type of curve?
  curve_row <- curve_sheet %>% filter(Name == analyte_name)
  caltype = curve_row$Cal_Type
  ## Parse. This should work for all formulas
  formula_text <- gsub("E","e",curve_row$Cal_Fn)
  formula_text <- gsub("=","~",formula_text)
  cal_formula <- as.formula(formula_text)
  return(cal_formula)
}
