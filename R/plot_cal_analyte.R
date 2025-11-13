#' Plot the calibration for a target analyte
#'
#' @description Plot calibration for a single ion species.
#'
#' 
#' @param parsed_calibration output from \code{parse_calibration()}.
#' @param analyte_name string matching an ion in the target calibration, e.g., "MSA".
#' @param axlim_offset_pct numeric; percentage the axes will be offset from the maximum and minimum value.
#' @param point_colour character; fill colour of points in calibration scatterplot. 
#' @param regline_colour character; colour of regression line
#' @param inset one of 'beside', 'inside' or NULL to plot the lowest 10% of values as a separate plot beside or inside the main plot, or not at all.
#'
#' @return a cowplot plotting object containing one or more ggplot2 plotting objects
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
#'
#' @export
#' 
plot_cal_analyte <- function(parsed_calibration,
                             analyte_name,
                             axlim_offset_pct = 1,
                             point_colour = 'black',
                             regline_colour = 'red',
                             inset = 'beside'){
  ## test vars
  # parsed_calibration <- cal_Anions
  # analyte_name <- "Fluoride"
  # axlim_offset_pct = 1
  # point_colour = 'black'
  # regline_colour = 'red'
  # inset = 'beside'
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
  analyte_reg <- get_cal_regression(parsed_calibration, analyte_name, geom_colour = regline_colour)
  ## Min, max from the data
  min_x <- min(analyte_data$Weight, na.rm = T)
  max_x <- max(analyte_data$Weight, na.rm = T)
  min_y <- min(analyte_data$Area, na.rm = T)
  max_y <- max(analyte_data$Area, na.rm = T)
  ## predict from model
  fit_data <- analyte_data %>% select(Weight, Area) %>% rename(x = Weight, y = Area)
  pred_y <- predict(analyte_reg, analyte_data %>% select(Weight) %>% rename(x = Weight))
  analyte_data <- analyte_data %>%
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
  
  # Get text size
  tsize <- theme_constants()
  ## Plots!
  main_plot <- ggplot() + 
    geom_line(data = analyte_data, aes(x = Weight, y = predicted_y)) +
    analyte_reg$geom +
    # geom_smooth(data = fit_data, aes(x = x, y = y), formula = y ~ poly(x, 3, raw = TRUE), method = 'lm', se = T) +
    geom_point(data = analyte_data, aes(x = Weight, y = Area), size = 2, shape = 21, fill = point_colour, colour = 'black') + 
    labs(x = "Level Weight (ppb)", y = "Area &micro;S*min") +
    scale_x_continuous(expand = c(0,0), 
                       limits = c(0,(max_x + ((max_x - min_x)/100)*axlim_offset_pct))) +
    scale_y_continuous(expand = c(0,0), 
                       limits = c(0,(max_y + ((max_y - min_y)/100)*axlim_offset_pct))) +
    theme_calplot(tsize$textsize_pdf)# +
  # geom_richtext(aes(x = (max_x - min_x)/100*15, y = (max_y - min_y)/100*5, label = model_regression), hjust = 0, fill = NA, label.size = NA, size = tsize$textsize_vvsmall/.pt)# +
  # geom_richtext(aes(x = (max_x - min_x)/100*80, y = (max_x - min_x)/100*10, label = fit_data), hjust = 0, fill = NA, label.size = NA, size = 6) 
  
  # Make inset
  inset_plot <- ggplot() + 
    geom_line(data = analyte_data, aes(x = Weight, y = predicted_y)) +
    analyte_reg$geom +
    # geom_smooth(data = fit_data, aes(x = x, y = y), formula = y ~ poly(x, 3, raw = TRUE), method = 'lm', se = T) +
    geom_point(data = analyte_data, aes(x = Weight, y = Area), size = 2, shape = 21, fill = point_colour, colour = 'black') + 
    labs(x = "Level Weight (ppb)", y = "Area &micro;S*min") +
    scale_x_continuous(expand = c(0,0), 
                       limits = c(-100,(max_x + ((max_x - min_x)/100)*axlim_offset_pct))) +
    scale_y_continuous(expand = c(0,0), 
                       limits = c(-100,(max_y + ((max_y - min_y)/100)*axlim_offset_pct))) +
    # theme_calplot(tsize$textsize_vvsmall) +
    theme(panel.background = element_rect(fill = 'white')) +
    coord_cartesian(xlim = c(0,(max_x - min_x)/100*10),
                    ylim = c(0,(max_y - min_y)/100*10))
  
  # Make simple plot for regression and labels
  
  if(inset == 'beside'){
    # Information
    # reg_info <- ggplot() + 
    #   scale_x_continuous(expand = c(0,0), limits = c(0,(max_x - min_x)/100*50)) +
    #   scale_y_continuous(expand = c(0,0),  limits = c(0,(max_y - min_y)/100*10)) +
    #   # Regression
    #   geom_richtext(aes(x = (max_x - min_x)/100*20.5, 
    #                     y = (max_y - min_y)/100*7.5, label = model_regression), hjust = 0, fill = NA, label.size = NA, size = tsize$textsize_small_pdf/.pt) + 
    #   geom_richtext(aes(x = (max_x - min_x)/100*20.5, 
    #                     y = (max_y - min_y)/100*2.5, label = paste0("R<sup>2</sup> = ", round(summary(analyte_reg)$r.squared,5))), hjust = 0, fill = NA, label.size = NA, size = tsize$textsize_small_pdf/.pt) + 
    #   # Analyte and calibration info
    #   geom_richtext(aes(x = (max_x - min_x)/100*0.5, 
    #                     y = (max_y - min_y)/100*7.5), 
    #                 label = paste0(analyte_name,", n = ",length(!is.na(analyte_data$Area)), ", ", parsed_calibration$Cal_Curves$Cal_Type[which(parsed_calibration$Cal_Curves$Name == analyte_name)]), 
    #                 hjust = 0, fill = NA, label.size = NA, size = tsize$textsize_small_pdf/.pt) + 
    #   geom_richtext(aes(x = (max_x - min_x)/100*0.5, 
    #                     y = (max_y - min_y)/100*2.5), 
    #                 label = paste0(parsed_calibration$Instrument,", ",paste0(parsed_calibration$Ion_Type," ",parsed_calibration$Number),", ", parsed_calibration$Date), 
    #                 hjust = 0, fill = NA, label.size = NA, size = tsize$textsize_small_pdf/.pt) + 
    #   geom_blank() +
    #   theme_blank
    # Update plots 
    inset_plot <- inset_plot + 
      theme_calplot(tsize$textsize_pdf) 
    main_plot <- main_plot + 
      geom_rect(aes(xmin = 0, xmax = (max_x - min_x)/100*10, ymin = 0, ymax = (max_y - min_y)/100*10), fill = NA, alpha = 0.5, colour = 'black')# +
      # annotate(geom = 'text', label = 'P2', fontface = 2,
      #          x = (max_x - min_x)/100*3, y = (max_y - min_y)/100*8) 
    # Combine
    # top_plot <- reg_info
    combined_plot <- plot_grid(main_plot,
                             inset_plot,
                             nrow = 1, ncol = 2, align = 'h')
    # combined_plot <- plot_grid(top_plot,
    #                            bottom_plot,
    #                            nrow = 2, ncol = 1,
    #                            rel_heights = c(0.15,1))
  } else if(inset == 'inside'){
    # Information
    # reg_info <- ggplot() + 
    #   scale_x_continuous(expand = c(0,0), limits = c(0,(max_x - min_x)/100*50)) +
    #   scale_y_continuous(expand = c(0,0),  limits = c(0,(max_y - min_y)/100*20)) +
    #   # Regression
    #   geom_richtext(aes(x = (max_x - min_x)/100*0.5, 
    #                     y = (max_y - min_y)/100*8, label = model_regression), hjust = 0, fill = NA, label.size = NA, size = tsize$textsize_small_pdf/.pt) + 
    #   geom_richtext(aes(x = (max_x - min_x)/100*0.5, 
    #                     y = (max_y - min_y)/100*4, label = paste0("R<sup>2</sup> = ", round(summary(analyte_reg)$r.squared,5))), hjust = 0, fill = NA, label.size = NA, size = tsize$textsize_small_pdf/.pt) + 
    #   # Analyte and calibration info
    #   geom_richtext(aes(x = (max_x - min_x)/100*0.5, 
    #                     y = (max_y - min_y)/100*16), 
    #                 label = paste0(analyte_name,", n = ",length(!is.na(analyte_data$Area)), ", ", parsed_calibration$Cal_Curves$Cal_Type[which(parsed_calibration$Cal_Curves$Name == analyte_name)]), 
    #                 hjust = 0, fill = NA, label.size = NA, size = tsize$textsize_small_pdf/.pt) + 
    #   geom_richtext(aes(x = (max_x - min_x)/100*0.5, 
    #                     y = (max_y - min_y)/100*12), 
    #                 label = paste0(parsed_calibration$Instrument,", ",paste0(parsed_calibration$Ion_Type," ",parsed_calibration$Number),", ", parsed_calibration$Date), 
    #                 hjust = 0, fill = NA, label.size = NA, size = tsize$textsize_small_pdf/.pt) + 
    #   geom_blank() +
    #   theme_blank
    # Update plots
    main_plot <- main_plot + 
      geom_rect(aes(xmin = 0, xmax = (max_x - min_x)/100*10, ymin = 0, ymax = (max_y - min_y)/100*10), fill = NA, alpha = 0.5, colour = 'black') +
      annotate(geom = 'text', label = 'inset', fontface = 2,
               x = (max_x - min_x)/100*15, y = (max_y - min_y)/100*5) 
    inset_plot <- inset_plot +
      theme_calplot(tsize$textsize_small_pdf) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    # Combine
    # top_plot <- reg_info
    combined_plot <- ggdraw() +
      draw_plot(main_plot) +
      draw_plot(inset_plot, x = 0.15, y = .58, width = .4, height = .4)
    # combined_plot <- plot_grid(top_plot,
    #                            bottom_plot,
    #                            nrow = 2, ncol = 1,
    #                            rel_heights = c(0.2,1))
  } else if(is.null(inset)){
    combined_plot <- main_plot
  }
  return(combined_plot)
}
