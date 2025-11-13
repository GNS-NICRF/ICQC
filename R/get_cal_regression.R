#' Obtain the regression for a single ion species.
#'
#' @description Replicate the regression used for the calibration of a target analyte.
#'
#' @param parsed_calibration output from \code{parse_calibration()}
#' @param analyte_name character string matching the target analyte. E.g., "MSA".
#' @param geom_se TRUE/FALSE to plot standard error
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
#' @importFrom stats lm
#'
#' @noRd
get_cal_regression <- function(parsed_calibration,
                               analyte_name,
                               geom_se = TRUE,
                               geom_extend = TRUE,
                               geom_colour = 'red'){
  ## Test vars
  # parsed_calibration = test_Anion
  # analyte_name = "MSA"
  # geom_se = TRUE
  # geom_extend = TRUE
  # geom_colour = 'red'
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
  if(grepl("Cubic", caltype)){
    if(grepl("WithOffset", caltype)){
      # Not through origin
      rep_model <- lm(y ~ poly(x, 3, raw = TRUE), data = fit_data)
      rep_model$geom <- list(
          geom_smooth(data = fit_data, aes(x = x, y = y), formula = y ~ poly(x, 3, raw = TRUE), se = geom_se, method = 'lm',
                      colour = geom_colour, fullrange = geom_extend))
    } else {
      # Assuming there's only through and not through origin here.
      # Through origin
      rep_model <- lm(y ~ poly(x, 3, raw = TRUE) - 1, data = fit_data)
      rep_model$geom <- list(
        geom_smooth(data = fit_data, aes(x = x, y = y), formula = y ~ poly(x, 3, raw = TRUE) - 1, se = geom_se, method = 'lm',
                    colour = geom_colour, fullrange = geom_extend))
    }
  } else if(grepl("Lin",caltype)){
    if(grepl("WithOffset", caltype)){
      # Not through origin
      rep_model <- lm(y ~ x, data = fit_data)
      rep_model$geom <- list(
        geom_smooth(data = fit_data, aes(x = x, y = y), formula = y ~ x, se = geom_se, method = 'lm',
                    colour = geom_colour, fullrange = geom_extend))
    } else {
      rep_model <- lm(y ~ x - 1, data = fit_data)
      rep_model$geom <- list(
        geom_smooth(data = fit_data, aes(x = x, y = y), formula = y ~ x - 1, se = geom_se, method = 'lm',
                    colour = geom_colour, fullrange = geom_extend))
    }
  }
  # Add in ggplot2 object
  return(rep_model)
}
