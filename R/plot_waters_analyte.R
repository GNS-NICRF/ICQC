#' Plot the calibration for a target analyte
#'
#' @description Plot calibration for a single ion species.
#'
#' @param analyte_sequence parsed output from an analytical run. Output from \code{parse_sequence()}.
#' @param analyte_name string matching an ion in the target calibration, e.g., "Sulphate".
#' @param axlim_offset_pct numeric; percentage the axes will be offset from the maximum and minimum value.
#' @param qc_point_colour character; fill colour of points on graph.
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
#'
#' @export
#'
plot_waters_analyte <- function(analyte_sequence,
                                 analyte_name,
                                 axlim_offset_pct = 1,
                                 point_colour = 'black'){
  ## test vars
  #
  analyte_sequence <-  parse_sequence(list.files(data_dir_raw_3, full.names = T)[2],
                                     ion_type = "Anion",
                                     verbose = FALSE,
                                     matching_cal_dir = data_dir)
  analyte_name <- "Nitrite"
  axlim_offset_pct = 1
  point_colour = ion_clrs[which(names(ion_clrs) == analyte_name)]
  # Input checks
  if(!is.character(analyte_name)){
    stop("param analyte_name must be a character.")
  }
  ## Prepare data, identify missing values of different types
  data_sp <- analyte_sequence$Data %>%
    filter(str_detect(Name, "Waters")) %>%
    # mutate(row_number = row_number()) %>%
    # mutate(sample_number = Name %>% str_extract(., "\\d+") %>% as.numeric()) %>%
    select(c(Name, contains(analyte_name))) %>%
    'colnames<-'(c('Name','Amount','Area')) %>%
    mutate(missing_type = case_when(
      is.na(Amount) & !is.na(Area) ~ "Missing amount",
      is.na(Amount) & is.na(Area)  ~ "Non-detect",
      TRUE                         ~ "Detected"
    )) %>%
    mutate(row_number = row_number())
  ## Find missing samples
  # if(!all(data_sp$missing_type == "Non-detect")){
  #   dummy_triangles <- data_sp %>%
  #     filter(!is.na(missing_type) & missing_type != "Detected") %>%
  #     mutate(colours = case_when(missing_type == "Missing amount" ~ "red2",
  #                                missing_type == "Non-detect" ~ "white")) %>%
  #     mutate(y = rep(0 + 0.02 * max(data_sp$Amount %>% as.numeric(), na.rm = T), nrow(.))) %>%
  #     rename(x = row_number) %>% select(x,y,missing_type,colours) %>%
  #     mutate(y = case_when(is.infinite(y) ~ NA))
  # } else {
  #   dummy_triangles <- data.frame()
  # }
  ## textsizes
  tsize <- theme_constants()
  # Plot
  data_plot <-  ggplot() +
    geom_vline(xintercept = seq(0,round(nrow(data_sp), digits = -1), 10), linetype = 'dashed',
               colour = 'grey60', alpha = 0.6) +
    # geom_step(data = data_sp, aes(x = row_number - 0.5, y = Amount)) +
    geom_point(data = data_sp, aes(x = row_number, y = Amount),
               shape = 21, colour = 'black', fill = point_colour) +
    scale_x_continuous(expand = c(0,0),
                       limits = c(0,nrow(data_sp)+1),
                       breaks = seq(0,round(nrow(data_sp), digits = -1), 10)) +
                       # labels = c('Start',data_sp$sample_number[seq(0,round(nrow(data_sp), digits = -1), 10)])) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, ifelse(all(data_sp$missing_type == "Non-detect") | all(is.na(data_sp$Amount)),
                                            yes = 1,
                                            no = max(data_sp$Amount, na.rm = T)*1.05))) + # manual 5% pad
    theme_resplot(font_size = tsize$textsize_pdf) +
    labs(y = paste0(analyte_name," (ppb)"), x = "Water injection in sequence") +
    theme(legend.title = element_blank(),
          plot.margin = margin(6,18,6,6))
  if(all(is.na(data_sp$Amount))){
    data_plot <- data_plot +
      annotate(geom = "text", x = 1 + ((max(data_sp$row_number) - 1)/100) * 50, y = 0.5, label = "Analyte not detected in any water injections")
  } #else if(nrow(dummy_triangles) == 0){
    #data_plot <- data_plot +
    #  annotate(geom = "text", x = 1 + ((max(data_sp$row_number) - 1)/100) * 50, y = 0.5, label = "Analyte not detected in any water injections")
  #}
  return(data_plot)
}
