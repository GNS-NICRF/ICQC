#' Obtain the regression for a single ion species.
#'
#' @description Replicate the regression used for the calibration of a target analyte.
#'
#' @param parsed_calibration output from \code{parse_calibration()}
#' @param analyte_name character string matching the target analyte. E.g., "MSA".
#' @param geom_se TRUE/FALSE to plot standard error
#' @param geom_extend TRUE/FALSE to extend geometry beyond fitting points.
#' @param geom_colour supply colour for regression line geom output.
#'
#' @return an object of class 'lm'. See \code{stats::lm()}
#'
#' @author Matt Harris
#'
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#'
#' @noRd
get_cal_regression <- function(parsed_calibration,
                               analyte_name,
                               geom_se = TRUE,
                               geom_extend = TRUE,
                               geom_colour = 'red'){
  ## Test vars
  # parsed_calibration = cal_Anions
  # analyte_name = "Fluoride"
  # geom_se = TRUE
  # geom_extend = TRUE
  # geom_colour = 'green'
  # Curve sheet
  curve_sheet <- parsed_calibration$Cal_Curves
  # What type of curve?
  curve_row <- curve_sheet %>% filter(Name == analyte_name)
  caltype = curve_row$Cal_Type
  # data
  levels_data <- parsed_calibration$Cal_Levels %>%
    'colnames<-'(c(gsub("Amount", "Weight", colnames(.)))) %>%
    dplyr::select(Injection_Number,Level,contains(analyte_name)) %>%
    mutate(across(everything(), ~as.numeric(.)))
  analyte_data <- parsed_calibration$Data %>%
    select(Injection_Number,Level, contains(analyte_name)) %>%
    mutate(across(everything(), ~as.numeric(.))) %>%
    left_join(levels_data) %>% # join
    'colnames<-'(c(colnames(.)[c(1,2)], "Amount","Area","Weight"))
  # Get data for fit
  fit_data <- analyte_data %>% select(Weight, Area) %>% rename(x = Weight, y = Area)
  # For different types
  # The logic tree for this is nested in curve_selector.
  rep_model <- curve_selector(caltype,
                              fit_data,
                              geom_se,
                              geom_extend,
                              geom_colour)
  # add analyte data just in case
  rep_model[["analyte_data"]] <- analyte_data
  # Add in ggplot2 object
  return(rep_model)
}
