#' Plot the calibration for a target analyte
#'
#' @description Plot calibration for a single ion species.
#'
#' @param analyte_sequence parsed output from an analytical run. Output from \code{parse_sequence()}.
#' @param analyte_name string matching an ion in the target calibration, e.g., "Sulphate".
#' @param axlim_offset_pct numeric; percentage the axes will be offset from the maximum and minimum value.
#' @param regline_colour character; colour of regression line
#' @param qc_point_colour character; fill colour of points for QCs added to scatterplot.
#'
#' @return a ggplot2 plotting object
#'
#' @author Matt Harris
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom stringi stri_locate_all_fixed
#' @importFrom stringr str_replace_all
#' @importFrom cowplot plot_grid
#' @importFrom cowplot ggdraw
#' @importFrom ggtext geom_richtext
#' @importFrom ggrepel geom_text_repel
#'
#' @export
#'
plot_qc_analyte <- function(analyte_sequence,
                            analyte_name,
                            axlim_offset_pct = 1,
                            regline_colour = 'red',
                            qc_point_colour = 'white'){
  ## test vars
  # analyte_sequence <-  parse_sequence(list.files(data_dir_raw_3, full.names = T)[1],
  #                                    ion_type = "Anion",
  #                                    verbose = FALSE,
  #                                    matching_cal_dir = data_dir)
  # analyte_name <- "Fluoride"
  # axlim_offset_pct = 1
  # qc_point_colour = ion_clrs[which(names(ion_clrs) == analyte_name)]
  # regline_colour = ion_clrs[which(names(ion_clrs) == analyte_name)]
  # Extract the relevant bits
  ## PREPARE CALIBRATION DATA
  parsed_calibration <- analyte_sequence$Matching_Calibration
  if(!is.character(analyte_name)){
    stop("param analyte_name must be a character.")
  }
  ## Combine the instrument-calculated ppb ('amounts') with the area and cal weights
  levels_data <- parsed_calibration$Cal_Levels %>%
    'colnames<-'(c(gsub("Amount", "Weight", colnames(.)))) %>%
    select(Injection_Number,Level,contains(analyte_name)) %>%
    mutate(across(everything(), ~as.numeric(.)))
  cal_data <- parsed_calibration$Data %>%
    select(Injection_Number,Level, contains(analyte_name)) %>%
    mutate(across(everything(), ~as.numeric(.))) %>%
    left_join(levels_data) %>% # join
    'colnames<-'(c(colnames(.)[c(1,2)], "Amount","Area","Weight"))
  ## Obtain calibration curve info
  cal_reg <- get_cal_regression(parsed_calibration, analyte_name, geom_colour = regline_colour)
  ## Min, max from the data
  min_x <- min(cal_data$Weight, na.rm = T)
  max_x <- max(cal_data$Weight, na.rm = T)
  min_y <- min(cal_data$Area, na.rm = T)
  max_y <- max(cal_data$Area, na.rm = T)
  ## predict from model
  fit_data <- cal_data %>% select(Weight, Area) %>% rename(x = Weight, y = Area)
  pred_y <- predict(cal_reg, cal_data %>% select(Weight) %>% rename(x = Weight))
  cal_data <- cal_data %>%
    mutate(predicted_y = pred_y)
  ## This block formats the regression text. All to turn ^3 into a properly displayed exponent
  model_regression = parsed_calibration$Cal_Curves$Cal_Fn[which(parsed_calibration$Cal_Curves$Name == analyte_name)]
  # find and replace exponents
  reg_exp_ind <- stringi::stri_locate_all_fixed(model_regression, pattern = "^")[[1]][,1] %>% as.numeric()
  if(all(!is.na(reg_exp_ind))){
    reg_exp_rep <- sapply(reg_exp_ind, function(x){substr(model_regression,x,x+1)}) %>%
      str_replace_all(.,"[\\^]","<sup>") %>%
      paste0(.,"</sup>")
    # Split da string
    for(i in seq_along(reg_exp_ind)){
      reg_exp_ind <- stri_locate_all_fixed(model_regression, pattern = "^")[[1]][,1] %>% as.numeric()
      this_split <- strsplit(model_regression,substr(model_regression,reg_exp_ind,reg_exp_ind+1), fixed = T)
      model_regression <- paste0(this_split[[1]][1],reg_exp_rep[i],this_split[[1]][2])
    }
    # Replace all excessive whitespace
  }
  model_regression <- str_replace_all(model_regression,"\\s+\\*\\s+","*")
  ## PREPARE QC DATA
  ion_type = analyte_sequence$Ion_Type
  anion_ion_sp <- unique(str_remove_all(
    colnames(analyte_sequence$Data[,grep("Amount",colnames(analyte_sequence$Data))[1]:ncol(analyte_sequence$Data)]),
    "_Amount|_Area"))
  # Get text size
  tsize <- theme_constants()
  # Isolate QCs
  QC_data <- analyte_sequence$Data %>%
    filter(str_detect(Name, "QC")) %>%
    filter(str_detect(Name, ion_type)) %>%
    select(Name, Level, contains(analyte_name)) %>%
    'colnames<-'(c('Name','Level','Amount','Area'))
  if(all(is.na(QC_data$Amount)) & all(is.na(QC_data$Area))){
    # warning("No QCs exhibit Area or Amount data for ",analyte_name)
    only_cal <- ggplot() +
      geom_line(data = cal_data, aes(x = Weight, y = predicted_y)) +
      cal_reg$geom +
      # geom_smooth(data = fit_data, aes(x = x, y = y), formula = y ~ poly(x, 3, raw = TRUE), method = 'lm', se = T) +
      geom_point(data = cal_data, aes(x = Weight, y = Area), size = 2, shape = 16, colour = 'black') +
      labs(x = "Weight/Amount (ppb)", y = "Area &micro;S*min") +
      scale_x_continuous(expand = c(0,0),
                         limits = c(0,(max_x + ((max_x - min_x)/100)*axlim_offset_pct))) +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,(max_y + ((max_y - min_y)/100)*axlim_offset_pct))) +
      theme_calplot(tsize$textsize_pdf) +
      annotate(geom = "text", x = min_x + ((max_x - min_x)/100) * 20, y = max_y - ((max_y - min_y)/100) * 10, label = "All QCs are non-detects")
    return(only_cal)
  } else {
    # dummy data to prevent label clipping
    fit <- lm(Area ~ Amount, data = QC_data)
    line_grid <- data.frame(Amount = seq(0, max(QC_data$Amount), length.out = 200))
    line_grid$Area <- predict(fit, newdata = line_grid)
    line_grid$Name <- ""
    line_grid$Level <- ""
    QC_data_forlabels <- QC_data %>% distinct(Level, .keep_all = T)

    ## Plots!
    cal_point_colour = 'black'
    main_plot <- ggplot() +
      geom_line(data = cal_data, aes(x = Weight, y = predicted_y)) +
      cal_reg$geom +
      # geom_smooth(data = fit_data, aes(x = x, y = y), formula = y ~ poly(x, 3, raw = TRUE), method = 'lm', se = T) +
      labs(x = "Weight/Amount (ppb)", y = "Area &micro;S*min") +
      scale_x_continuous(expand = c(0,0),
                         limits = c(0,(max_x + ((max_x - min_x)/100)*axlim_offset_pct))) +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,(max_y + ((max_y - min_y)/100)*axlim_offset_pct))) +
      theme_calplot(tsize$textsize_pdf) +
      # geom_point(data = QC_data  %>% mutate(Level = as.character(Level)) %>% full_join(line_grid), aes(x = Amount, y = Area), alpha = 1) +
      geom_smooth(data = QC_data, aes(x = Amount, y = Area),
                  method = 'lm', fullrange = T, colour = 'grey60', linetype = 'solid', alpha = 0.5) +
      geom_point(data = cal_data, aes(x = Weight, y = Area), size = 3, shape = 3, colour = 'black') +
      geom_point(data = QC_data, aes(x = Amount, y = Area),
                 shape = 23, colour = 'black', fill = qc_point_colour, size = 3) +
      geom_text_repel(data = QC_data_forlabels %>% mutate(Level = as.character(Level)) %>% full_join(line_grid),
                      aes(x = Amount, y = Area, label = Level),
                      force = 5, nudge_y = max(QC_data$Area, na.rm = T) * 0.1)
    # geom_richtext(aes(x = (max_x - min_x)/100*15, y = (max_y - min_y)/100*5, label = model_regression), hjust = 0, fill = NA, label.size = NA, size = tsize$textsize_vvsmall/.pt)# +
    # geom_richtext(aes(x = (max_x - min_x)/100*80, y = (max_x - min_x)/100*10, label = fit_data), hjust = 0, fill = NA, label.size = NA, size = 6)
    return(main_plot)
  }
}
