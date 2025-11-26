#' Residual plot of a single analyte around a regression
#'
#' @description Plot the residuals of a single analyte relative to its versus-known-weight regression in microsiemens per minute (peak area).
#'
#' @param imported_spreadsheet spreadsheet imported containing per-level and per-analyte ppb weights and instrument-returned values.
#' @param analyte_name Exact character name for analyte. Will be used for explicit character matching. E.g., 'Lithium'.
#' @param unit one of either 'area' for raw residuals values in microsiemens per minute or in the same units, but expressed as a percentage of the measured value.
#' @param axlim_offset_pct numeric; percentage value to offset limits of the y axis beyond the min and max values. Default is 1.
#' @param point_colour Fill colour for points.
#'
#' @return A ggplot object.
#'
#' @author Matt Harris
#'
#' @export
#'
plot_res_analyte <- function(parsed_calibration,
                             analyte_name,
                             unit = "percent",
                             axlim_offset_pct = 1,
                             point_colour = 'black'){
  ## test vars
  # parsed_calibration <- test_Anion
  # analyte_name <- "Fluoride"
  # axlim_offset_pct = 1
  # unit = "percent"
  # point_colour = 'black'

  # Extract the relevant bits
  instrument <- parsed_calibration$Instrument
  version <- paste0(parsed_calibration$Ion_Type," ",parsed_calibration$Number)
  # Quick input testing
  if(!is.data.frame(parsed_calibration)){
    data <- parsed_calibration$Data
  }
  if(!is.character(analyte_name)){
    stop("param analyte_name must be a character.")
  }
  ## Combine the instrument-calculated ppb ('amounts') with the area and cal weights
  levels_data <- parsed_calibration$Cal_Levels %>%
    'colnames<-'(c(gsub("Amount", "Weight", colnames(.)))) %>%
    select(Injection_Number,Level,contains(analyte_name)) %>%
    mutate(across(everything(), ~as.numeric(.)))
  analyte_data <- parsed_calibration$Data %>%
    select(Injection_Number,Level, contains(analyte_name)) %>%
    mutate(across(everything(), ~as.numeric(.))) %>%
    left_join(levels_data) %>% # join
    'colnames<-'(c(colnames(.)[c(1,2)], "Amount","Area","Weight"))
  ## Obtain calibration curve info
  analyte_reg <- get_cal_regression(parsed_calibration, analyte_name, geom_colour = 'black')
  ## Account for missing values
  miss_val_pos <- which(is.na(analyte_reg$analyte_data$Area))
  res_vec <- analyte_reg$residuals
  for(v in seq_along(miss_val_pos)){
    # print(v)
    res_vec <- append(res_vec,NA, after = miss_val_pos[v]-1)
  }
  Residual_frame <- res_vec %>%
    as.data.frame() %>% 'colnames<-'(c('Residuals')) %>%
    mutate(Level = analyte_reg$analyte_data$Level) %>% relocate(Level) %>%
    mutate(Area = analyte_reg$analyte_data$Area,
           Amount = analyte_reg$analyte_data$Amount,
           Weight = analyte_reg$analyte_data$Weight) %>%
    mutate(res_pct = Residuals/Area*100)

  # How to implement option for pct
  if(unit == "percent"){
    # Area, percent
    min_y <- min(Residual_frame$res_pct, na.rm = T)
    max_y <- max(Residual_frame$res_pct, na.rm = T)
    data_plot <- ggplot(data = Residual_frame, aes(x = Level, y = res_pct)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = seq(1,20,1), colour = 'grey60', alpha = 0.2, linetype = 'dashed') +
      geom_point(shape = 21, colour = 'black', fill = point_colour) +
      scale_x_continuous(expand = c(0,0), limits = c(0,21),
                         breaks = seq(1,20,1),
                         labels = seq(1,20,1)) +
      scale_y_continuous(expand = c(0,0), limits = c(ifelse(sign(min_y) == -1,
                                                            yes = min_y + (min_y * axlim_offset_pct/100),
                                                            no = min_y - (min_y * axlim_offset_pct/100)),
                                                     ifelse(sign(max_y) == 1,
                                                            yes = max_y + (max_y * axlim_offset_pct/100),
                                                            no = max_y - (max_y * axlim_offset_pct/100)))) +
      theme_cowplot(12) +
      theme(legend.position = 'inside',
            plot.title = element_text(hjust = 0.5)) +
      labs(x = "Level",
           y = paste0("Residuals (%, uS/min)"))
  } else if(unit == "area"){
    # Area
    min_y <- min(Residual_frame$Residuals, na.rm = T)
    max_y <- max(Residual_frame$Residuals, na.rm = T)
    data_plot <- ggplot(data = Residual_frame, aes(x = Level, y = Residuals)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = seq(1,20,1), colour = 'grey60', alpha = 0.2, linetype = 'dashed') +
      geom_point(shape = 21, colour = 'black', fill = point_colour) +
      scale_x_continuous(expand = c(0,0), limits = c(0,21),
                         breaks = seq(1,20,1),
                         labels = seq(1,20,1)) +
      scale_y_continuous(expand = c(0,0), limits = c(ifelse(sign(min_y) == -1,
                                                            yes = min_y + (min_y * axlim_offset_pct/100),
                                                            no = min_y - (min_y * axlim_offset_pct/100)),
                                                     ifelse(sign(max_y) == 1,
                                                            yes = max_y + (max_y * axlim_offset_pct/100),
                                                            no = max_y - (max_y * axlim_offset_pct/100)))) +
      theme_cowplot(12) +
      theme(legend.position = 'inside',
            plot.title = element_text(hjust = 0.5)) +
      labs(x = "Level",
           y = paste0("Residuals (uS/min)"))
  }
  return(data_plot)
}
