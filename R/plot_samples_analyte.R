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
#' @importFrom forcats fct_inorder
#'
#' @export
#'
plot_samples_analyte <- function(analyte_sequence,
                                 analyte_name,
                                 axlim_offset_pct = 1,
                                 point_colour = 'black'){
  ## test vars
  #
  # analyte_sequence <-  parse_sequence(list.files(data_dir_raw_3, full.names = T)[1],
  #                                    ion_type = "Cation",
  #                                    verbose = FALSE,
  #                                    matching_cal_dir = data_dir)
  # analyte_name <- "Lithium"
  # axlim_offset_pct = 1
  # point_colour = ion_clrs[which(names(ion_clrs) == analyte_name)]
  # Input checks
  if(!is.character(analyte_name)){
    stop("param analyte_name must be a character.")
  }
  ## Prepare data, identify missing values of different types
  data_sp <- analyte_sequence$Data %>%
    filter(!str_detect(Name, "Waters|QC|AES")) %>%
    # mutate(row_number = row_number()) %>%
    mutate(sample_number = Name %>% str_extract(., "\\d+") %>% as.numeric()) %>%
    select(c(Name,sample_number, contains(analyte_name))) %>%
    'colnames<-'(c('Name','sample_number','Amount','Area')) %>%
    mutate(missing_type = case_when(
      is.na(Amount) & !is.na(Area) ~ "Missing amount",
      is.na(Amount) & is.na(Area)  ~ "Non-detect",
      TRUE                         ~ NA_character_
    ))

  # Find missing samples
  all_nums <- seq(data_sp$sample_number[1],data_sp$sample_number[length(data_sp$sample_number)],1)
  missing_nums <- setdiff(all_nums,data_sp$sample_number)
  prefix <- str_remove(data_sp$Name[1], "\\d+$")
  # Create missing rows
  missing_rows <- tibble(
    Name = paste0(prefix, sprintf("%03d", missing_nums)),
    sample_number = missing_nums,
    Amount = NA,
    Area = NA,
    missing_type = "Missing sample"
  )
  # Bind
  data_sp <- bind_rows(data_sp,missing_rows) %>%
    mutate(row_number = row_number())
  # Missing val triangles
  dummy_triangles <- data_sp %>%
    filter(!is.na(missing_type)) %>%
    mutate(colours = case_when(missing_type == "Missing amount" ~ "red2",
                               missing_type == "Non-detect" ~ "white",
                               missing_type == "Missing sample" ~ "black")) %>%
    mutate(y = rep(0 + 0.02 * max(data_sp$Amount %>% as.numeric(), na.rm = T), nrow(.))) %>%
    rename(x = row_number) %>% select(x,y,missing_type,colours)
  # textsizes
  tsize <- theme_constants()
  # Plot
  # if(all(is.na(data_sp$Amount)) & all(is.na(data_sp$Area))){
    data_plot <-  ggplot() +
      geom_vline(xintercept = seq(0,round(nrow(data_sp), digits = -1), 10), linetype = 'dashed',
                 colour = 'grey60', alpha = 0.6) +
      geom_step(data = data_sp, aes(x = row_number - 0.5, y = Amount)) +
      geom_point(data = data_sp, aes(x = row_number, y = Amount),
                 shape = 21, colour = 'black', fill = point_colour) +
      scale_x_continuous(expand = c(0,0),
                         limits = c(0,nrow(data_sp)+1),
                         breaks = seq(0,round(nrow(data_sp), digits = -1), 10),
                         labels = c('Start',data_sp$sample_number[seq(0,round(nrow(data_sp), digits = -1), 10)])) +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,ifelse(all(is.na(data_sp$Amount)) & all(is.na(data_sp$Area)),
                                             yes = 1,
                                             no = max(data_sp$Amount, na.rm = T)*1.05))) + # manual 5% pad
      theme_resplot(font_size = tsize$textsize_pdf) +
      labs(y = paste0(analyte_name," (ppb)"), x = "Sample #") +
      theme(legend.title = element_blank())
  # }
  if(all(is.na(data_sp$Amount)) & all(is.na(data_sp$Area))){
    data_plot <- data_plot +
      annotate(geom = "text", x = 1 + ((max(data_sp$row_number) - 1)/100) * 50, y = 0.5, label = "Analyte not detected in any unknown injections")
  } else if(nrow(dummy_triangles) != 0){
    colormap <- setNames(dummy_triangles$colours, dummy_triangles$missing_type)
    labelmap <- setNames(dummy_triangles$missing_type, dummy_triangles$missing_type)
    data_plot <- data_plot +
      # geom_vline(xintercept = dummy_triangles$x, colour = ifelse(dummy_triangles$colours == "white", yes = "black", no = dummy_triangles$colours)) +
      geom_point(data = dummy_triangles, aes(x = x, y = y, fill = fct_inorder(missing_type)), shape = 24, colour = 'black', size = 2) +
      scale_fill_manual(values = colormap, labels = labelmap)
  }
  return(data_plot)
}
