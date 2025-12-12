# Experimenting with calibration curve optimisation

# Last updated YYYY-MM-DD
# Matt Harris
# m.harris@gns.cri.nz
# https://www.github.com/MRPHarris

# Written and run in:
#   Rstudio v2024.12.1
#   R v4.4.0
#   Windows 11 (Enterprise)

##### SETUP #####

## Install/uninstall lines
# detach('package:ecmwfr', unload = TRUE)
# installr::uninstall.packages('ecmwfr')
# install.packages('ecmwfr')
# devtools::install_github('MRPHarris/PACKAGENAME')

## Required packages
library(pacman)
pacman::p_load(tidyverse, dplyr, magrittr, here,
               ggtext, readxl, rmarkdown, stats, stringi, stringr)

# Notes on setup
# Nothing to see here
proj_dir <- paste0(here(),"/")
#setwd(wd)

# Source functions from a package
scripts_dir <- paste0(proj_dir,"R/")
invisible(sapply(list.files(scripts_dir, full.names = TRUE),
                 function(x){source(x)}))

data_dir <- paste0(proj_dir,"data-raw/test-data-2025-10-16/")

##### Get a curve for an example analyte #####

## Section notes
# Get a curve, yeah
example_files <- list.files(data_dir, full.names = T)
cal_Anions <- parse_sequence(filepath = example_files[1], ion_type = "Anion")

# Just one ion for now
ion = "MSA"
## Data etc
analyte_reg <- get_cal_regression(cal_Anions, ion)
analyte_data_nowgt <- cal_Anions$Data %>%
  select(Injection_Number,Level, contains(ion)) %>%
  mutate(across(everything(), ~as.numeric(.))) %>%
  'colnames<-'(c(colnames(.)[c(1,2)], "Amount","Area"))#,"Weight"))

# So, what are we after here?
# - ideally, a per-point statistic of how important each one is for maintaining the overall shape of the curve.
# - but, any number of points can be removed.

# Iterate through each point, and remove it individually, along with every other point. So, find n combinations of
# removals

## Ok, a few things need to happen.

# I need to find a way to produce optimised regressions for any analyte, minimising residuals.
# Curves:
# - linear
# - quadratic (2nd-order polynomial)
# - cubic (3rd-order polynomial)
# With options:
# - through origin
# - 1/x











# Can this calibration be improved?
# Get analyte data and coerce to mean of each level
analyte_data_mn <- analyte_reg$analyte_data %>%
  group_by(Level) %>%
  summarise(across(c(Amount,Area,Weight), ~ mean(., na.rm = T)))

find_best_curve <- function(cal_regression){
  cal_regression <- analyte_reg
  analyte_data_mn <- cal_regression$analyte_data %>%
    group_by(Level) %>%
    summarise(across(c(Amount,Area,Weight), ~ mean(., na.rm = T)))

  # iterate through, fitting each curve. Then find best.
  model_types <- c("poly3","poly2","linear")
  force_origin <- c(TRUE,FALSE) %>% as.logical()
  inverse_x <- c(TRUE,FALSE) %>% as.logical()

  parameter_combinations <- expand.grid(model_types,force_origin,inverse_x, stringsAsFactors = F) %>%
    'colnames<-'(c('model_type','force_origin','inverse_x'))



}

curve_type <- "poly3"

##### Fns #####

test_fit <- fit_model(x = cal_regression$analyte_data$Weight,
                      y = cal_regression$analyte_data$Area,
                      model = c("poly3","poly2","linear"),
                      weights = NULL,
                      force_origin = FALSE,
                      inverse_x = FALSE)

fit_model <- function(x, y,
                      model = c("poly3","poly2","linear"),
                      weights = NULL,
                      force_origin = FALSE,
                      inverse_x = FALSE){
  model <- match.arg(model)
  n <- length(x)
  out <- list()

  if(inverse_x){
    if(any(x == 0)){
      stop("transform_invx = TRUE requires x != 0 (cannot compute 1/x).")
    }
    x_trans <- 1 / x
    x_name <- 'invx'
  } else {
    x_trans <- x
    x_name <- 'x'
  }
  if (model %in% c("linear","poly2","poly3")) {
    deg <- switch(model, linear = 1, poly2 = 2, poly3 = 3)
    df <- data.frame(x = x, y = y)
    # raw polynomial to keep coefficients interpretable
    poly_terms <- paste0("I(", x_name, "^", 1:deg, ")")
    rhs <- paste(poly_terms, collapse = " + ")
    # Force through origin by removing intercept with -1
    formula_str <- if (force_origin) paste("y ~ -1 +", rhs) else paste("y ~", rhs)
    formula <- as.formula(formula_str)
    if (is.null(weights)) {
      fit <- lm(formula, data = df)
    } else {
      w <- as.numeric(weights)
      if (any(w <= 0)) stop("Weights must be positive.")
      fit <- lm(formula, data = df, weights = w)
    }
    # predictor function on new x for future use
    f <- function(z) {
      predict(fit, newdata = data.frame(x = z))
    }
    p <- length(coef(fit))
    # influence diagnostics
    # dffits_vals <- dffits(fit)
    out$predict <- f
    out$diagnostics <- list(
      coef = coef(fit), # get model coefficients
      n_coef = p, # get n coefficients
      yhat = fitted(fit), # fitted values
      resid = residuals(fit), # model residuals
      leverage = hatvalues(fit), # per-point leverage (in x space, I think?)
      mse = sum(resid^2) / max(n - p, 1), # mean squared error
      cooks = cooks.distance(fit), # combined effect of leverage and residuals. Higher = greater point influence
      model_type = model,
      force_origin = force_origin,
      inverse_x = inverse_x
    )
    return(out)
  } else {
    stop("Unknown model: ", model)
  }
}


##### Fitting #####

# Remove each single point and generate new curves.
# Generate combination of all points? How to re-map this back onto the original curve?
n_levels <- length(unique(analyte_reg$analyte_data$Level)) %>% as.numeric()



lcombs <- t(combn(seq(2,n_levels-1,1),8)) %>% as.data.frame()

# Combinations
min_n_levels <- 8
comb_list <- vector('list',length = n_levels - min_n_levels)
for(i in seq_along(comb_list)){comb_list[[i]] <- analyte_data_mn$Level}
# comb_list[] <- as.matrix(analyte_data_mn$Level)
level_combs <- expand.grid(comb_list)


curve_list <- vector('list', length = n_levels)
for(l in seq_along(n_levels)){
  new_curve =

}


