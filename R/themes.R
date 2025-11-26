# Themes for plotting

#' Theme for residual plots
#'
#' @description A ggplot2 theme object for use within calibration residual plots
#'
#' Ref2
#' @param font_size size of font in pt.
#' @param font_family family of font
#' @param line_size size of main lines
#' @param rel_small scalar for relative size of small text items
#' @param rel_tiny scalar for relative size of tiny text items
#' @param rel_large scalar for relative size of large text items
#'
#' @return a ggplot2 theme object
#'
#' @author Matt Harris
#'
#' @importFrom ggplot2 theme
#' @importFrom ggtext element_markdown
#'
#' @noRd
#'
theme_resplot <- function(font_size, font_family = "", line_size = 1,
                          rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14){
  half_line <- font_size/2
  small_size <- rel_small * font_size
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    ggplot2::theme(line = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
                   rect = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
                   text = element_text(family = font_family, face = "plain", color = "black", size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(), debug = FALSE),
                   axis.line = element_line(color = "black", size = line_size, lineend = "square"),
                   axis.line.x = element_line(color = "black", size = line_size, lineend = "square"),
                   axis.line.y = element_line(color = "black", size = line_size, lineend = "square"),
                   axis.text = element_text(color = "black", size = small_size),
                   axis.text.x = element_text(margin = margin(t = small_size/4), vjust = 1),
                   axis.text.x.top = element_text(margin = margin(b = small_size/4),  vjust = 0),
                   axis.text.y = element_text(margin = margin(r = small_size/4), hjust = 1),
                   axis.text.y.right = element_text(margin = margin(l = small_size/4), hjust = 0),
                   axis.ticks = element_line(color = "black",  size = line_size),
                   axis.ticks.length = unit(half_line/2, "pt"),
                   axis.title.x = element_markdown(margin = margin(t = half_line/2), vjust = 1),#margin = margin(t = half_line/2), vjust = 1),
                   axis.title.x.top = element_markdown(margin = margin(b = half_line/2), vjust = 0),#margin = margin(b = half_line/2), vjust = 0),
                   axis.title.y = element_markdown(angle = 90, margin = margin(r = half_line/2), vjust = 1),
                   axis.title.y.right = element_markdown(angle = -90, margin = margin(l = half_line/2), vjust = 0),
                   axis.title.y.left = element_markdown(angle = 90, margin = margin(r = half_line/2), vjust = 1),
                   legend.background = element_blank(),
                   legend.spacing = unit(font_size, "pt"), legend.spacing.x = NULL,
                   legend.spacing.y = NULL, legend.margin = margin(0, 0, 0, 0),
                   legend.key = element_blank(),
                   legend.key.size = unit(1.1 * font_size, "pt"),
                   legend.key.height = NULL,
                   legend.key.width = NULL,
                   legend.text = element_text(size = rel(rel_small)),
                   legend.text.align = NULL,
                   legend.title = element_text(hjust = 0),
                   legend.title.align = NULL,
                   legend.position = "right",
                   legend.direction = NULL,
                   legend.justification = c("left", "center"),
                   legend.box = NULL,
                   legend.box.margin = margin(0, 0, 0, 0),
                   legend.box.background = element_blank(),
                   legend.box.spacing = unit(font_size, "pt"),
                   panel.border = element_blank(),
                   panel.grid = element_blank(),
                   # panel.grid.major = element_line(color = 'grey60', linetype = 'solid'),
                   panel.grid.minor = NULL,
                   # panel.grid.major.x = element_line(color = 'grey60', linetype = 'solid'),
                   # panel.grid.major.y = element_line(color = 'grey60', linetype = 'solid'),
                   panel.background = element_blank(),
                   panel.grid.minor.x = NULL, panel.grid.minor.y = NULL,
                   panel.spacing = unit(half_line, "pt"), panel.spacing.x = NULL,
                   panel.spacing.y = NULL, panel.ontop = FALSE, strip.background = element_rect(fill = "grey80"),
                   strip.text = element_text(size = rel(rel_small),margin = margin(half_line/2, half_line/2, half_line/2, half_line/2)),
                   strip.text.x = NULL,
                   strip.text.y = element_text(angle = -90),
                   strip.placement = "inside", strip.placement.x = NULL,
                   strip.placement.y = NULL, strip.switch.pad.grid = unit(half_line/2,"pt"),
                   strip.switch.pad.wrap = unit(half_line/2,"pt"),
                   plot.background = element_blank(),
                   plot.title = element_blank(),
                   plot.subtitle = element_text(size = rel(rel_small),hjust = 0, vjust = 1, margin = margin(b = half_line)),
                   plot.caption = element_text(size = rel(rel_tiny),hjust = 1, vjust = 1, margin = margin(t = half_line)),
                   plot.tag = element_text(face = "bold", hjust = 0,vjust = 0.7),
                   plot.tag.position = c(0, 1),
                   plot.margin = margin(half_line,half_line, half_line, half_line),
                   complete = TRUE)
}


#' Theme for calibration plots
#'
#' @description A ggplot2 theme object for use within calibration scatter plots
#'
#' Ref2
#' @param font_size size of font in pt.
#' @param font_family family of font
#' @param line_size size of main lines
#' @param rel_small scalar for relative size of small text items
#' @param rel_tiny scalar for relative size of tiny text items
#' @param rel_large scalar for relative size of large text items
#'
#' @return a ggplot2 theme object
#'
#' @author Matt Harris
#'
#' @importFrom ggplot2 theme
#' @importFrom ggtext element_markdown
#'
#' @noRd
#'
theme_calplot <- function(font_size, font_family = "", line_size = 1,
                          rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14){
  half_line <- font_size/2
  small_size <- rel_small * font_size
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    ggplot2::theme(line = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
          rect = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
          text = element_text(family = font_family, face = "plain", color = "black", size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(), debug = FALSE),
          axis.line = element_line(color = "black", size = line_size, lineend = "square"),
          axis.line.x = element_line(color = "black", size = line_size, lineend = "square"),
          axis.line.y = element_line(color = "black", size = line_size, lineend = "square"),
          axis.text = element_text(color = "black", size = small_size),
          axis.text.x = element_text(margin = margin(t = small_size/4), vjust = 1),
          axis.text.x.top = element_text(margin = margin(b = small_size/4),  vjust = 0),
          axis.text.y = element_text(margin = margin(r = small_size/4), hjust = 1),
          axis.text.y.right = element_text(margin = margin(l = small_size/4), hjust = 0),
          axis.ticks = element_line(color = "black",  size = line_size),
          axis.ticks.length = unit(half_line/2, "pt"),
          axis.title.x = element_markdown(margin = margin(t = half_line/2), vjust = 1),#margin = margin(t = half_line/2), vjust = 1),
          axis.title.x.top = element_markdown(margin = margin(b = half_line/2), vjust = 0),#margin = margin(b = half_line/2), vjust = 0),
          axis.title.y = element_markdown(angle = 90, margin = margin(r = half_line/2), vjust = 1),
          axis.title.y.right = element_markdown(angle = -90, margin = margin(l = half_line/2), vjust = 0),
          axis.title.y.left = element_markdown(angle = 90, margin = margin(r = half_line/2), vjust = 1),
          legend.background = element_blank(),
          legend.spacing = unit(font_size, "pt"), legend.spacing.x = NULL,
          legend.spacing.y = NULL, legend.margin = margin(0, 0, 0, 0),
          legend.key = element_blank(),
          legend.key.size = unit(1.1 * font_size, "pt"),
          legend.key.height = NULL,
          legend.key.width = NULL,
          legend.text = element_text(size = rel(rel_small)),
          legend.text.align = NULL,
          legend.title = element_text(hjust = 0),
          legend.title.align = NULL,
          legend.position = "right",
          legend.direction = NULL,
          legend.justification = c("left", "center"),
          legend.box = NULL,
          legend.box.margin = margin(0, 0, 0, 0),
          legend.box.background = element_blank(),
          legend.box.spacing = unit(font_size, "pt"),
          panel.border = element_blank(), panel.grid = element_line(color = 'grey60', linetype = 'solid', size = 0.5),
          # panel.grid.major = element_line(color = 'grey60', linetype = 'solid'),
          panel.grid.minor = NULL,
          # panel.grid.major.x = element_line(color = 'grey60', linetype = 'solid'),
          # panel.grid.major.y = element_line(color = 'grey60', linetype = 'solid'),
          panel.background = element_blank(),
          panel.grid.minor.x = NULL, panel.grid.minor.y = NULL,
          panel.spacing = unit(half_line, "pt"), panel.spacing.x = NULL,
          panel.spacing.y = NULL, panel.ontop = FALSE, strip.background = element_rect(fill = "grey80"),
          strip.text = element_text(size = rel(rel_small),margin = margin(half_line/2, half_line/2, half_line/2, half_line/2)),
          strip.text.x = NULL,
          strip.text.y = element_text(angle = -90),
          strip.placement = "inside", strip.placement.x = NULL,
          strip.placement.y = NULL, strip.switch.pad.grid = unit(half_line/2,"pt"),
          strip.switch.pad.wrap = unit(half_line/2,"pt"),
          plot.background = element_blank(),
          plot.title = element_markdown(face = "bold",size = rel(rel_large), hjust = 0.5, vjust = 1,margin = margin(b = half_line)),
          plot.subtitle = element_text(size = rel(rel_small),hjust = 0, vjust = 1, margin = margin(b = half_line)),
          plot.caption = element_text(size = rel(rel_tiny),hjust = 1, vjust = 1, margin = margin(t = half_line)),
          plot.tag = element_text(face = "bold", hjust = 0,vjust = 0.7),
          plot.tag.position = c(0, 1),
          plot.margin = margin(half_line,half_line, half_line, half_line),
          complete = TRUE)
}


#' Blank theme
#'
#' @description To tack onto geom_blank()
#'
#' @return a ggplot2 theme object
#'
#' @author Matt Harris
#'
#' @importFrom ggplot2 theme
#'
#' @noRd
#'
theme_blank <- function(){
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    #axis.ticks.length.x = unit(3, "pt"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    #axis.line.x = element_line(size = 0.7, linetype = 'solid',colour = "black"),
    axis.line.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"))
}
