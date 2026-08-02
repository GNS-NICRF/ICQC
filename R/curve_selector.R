#' Return calibration curve model and geometry matching the supplied data.
#'
#' @description Replicate the regression used for the calibration of a target analyte.
#'
#' @param caltype The 'Cal.Type' string returned by Chromeleon that identifies the fit, offsetting, and weights associated with a calibration curve.
#' @param fit_data Data used to feed the regression. Generally x = analyte level weight, y = instrument response.
#' @param geom_se TRUE/FALSE to plot standard error
#' @param geom_extend TRUE/FALSE to extend geometry beyond fitting points.
#' @param geom_colour supply colour for regression line geom output.
#'
#' @return a nested object built upon the object type 'lm'. See \code{stats::lm()}. It also contains ggplot2 geometry and the input data.
#'
#' @author Matt Harris
#'
#' @importFrom stats lm
#' @importFrom stats poly
#' @importFrom stats update
#' @importFrom ggplot2 geom_smooth
#'
#' @noRd
curve_selector <- function(caltype,
                           fit_data,
                           geom_se,
                           geom_extend,
                           geom_colour){
  # Get the right calibration curve.
  # First, choose the right formula
  lin_formula <- y ~ x
  quad_formula <- y ~ x + I(x^2)
  cubic_formula <- y ~ poly(x, 3, raw = TRUE)
  # Choose regression type
  if(grepl("Lin",caltype)){
    reg_formula <- lin_formula
  } else if(grepl("Quad",caltype)){
    reg_formula <- quad_formula
  } else if(grepl("Cubic",caltype)){
    reg_formula <- cubic_formula
  } else {
    stop("Curve fit inside calibration type not recognised: '", caltype,"'. Check functions `get_cal_regression()` and `curve_selector()`.")
  }
  # Is it forced through the origin (i.e., no offset)?
  if(!grepl("WithOffset", caltype)){
    reg_formula <- update(reg_formula, paste("~ . -", 1))
  }
  # Now do the actual model building
  # ADD WEIGHTS (if there are any).
  # In this case, we check for the presence of (1) the weighting string 1/A, then (2) look for an exponent using perl.
  if(grepl("1/A", caltype) & grepl("\\p{No}", caltype, perl = TRUE)){
    rep_model <- lm(reg_formula, data = fit_data, weights = 1/fit_data$x^2)
    rep_model$geom <- list(
      geom_smooth(data = fit_data, aes(x = x, y = y, weight = 1/x^2), formula = reg_formula, se = geom_se, method = 'lm',
                  colour = geom_colour, fullrange = geom_extend))
  } else if(grepl("1/A", caltype) & !grepl("\\p{No}", caltype, perl = TRUE)){
    rep_model <- lm(reg_formula, data = fit_data, weights = 1/fit_data$x^2)
    rep_model$geom <- list(
      geom_smooth(data = fit_data, aes(x = x, y = y, weight = 1/x), formula = reg_formula, se = geom_se, method = 'lm',
                  colour = geom_colour, fullrange = geom_extend))
  } else {
    # No weights
    rep_model <- lm(reg_formula, data = fit_data)
    rep_model$geom <- list(
      geom_smooth(data = fit_data, aes(x = x, y = y), formula = reg_formula, se = geom_se, method = 'lm',
                  colour = geom_colour, fullrange = geom_extend))
  }
  return(rep_model)
}
