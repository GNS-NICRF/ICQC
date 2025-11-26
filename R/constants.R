# Constants

#' Function that defines constants for use in themes
#'
#' @description Returns a list containing a set of constants for use in themes
#'
#' @noRd
theme_constants <- function(){
  constants <- vector('list', length = 4)
  constants[[1]] <- 9 * 2.35
  constants[[2]] <- 8 * 2.3
  constants[[3]] <- 7 * 2.25
  constants[[4]] <- 6 * 2.2
  constants[[5]] <- 6 * 2
  constants[[6]] <- 5 * 1.5
  names(constants) <- c("textsize_normal","textsize_small","textsize_vsmall","textsize_vvsmall","textsize_pdf","textsize_small_pdf")
  return(constants)
}

#' Function that defines colours for ions.
#'
#' @description Returns a list containing colours for each ion.
#'
#' @noRd
ion_colors <- function(){
  clrs <- vector('list', length = 14)
  names(clrs) <- c("Fluoride","MSA","Chloride","Nitrite","Bromide","Nitrate","Sulphate","Phosphate",
                  "Lithium","Sodium","Ammonium","Potassium","Magnesium","Calcium")
  clrs$Fluoride <- "Forest Green"
  clrs$MSA <- "Dark Slate Blue"
  clrs$Chloride <- "Blue"
  clrs$Nitrite <- "Medium Slate Blue"
  clrs$Bromide <- "Brown"
  clrs$Nitrate <- "Dark Magenta"
  clrs$Sulphate <- "Dark Orange"
  clrs$Phosphate <- "Gold"
  clrs$Lithium <- "Navy Blue"
  clrs$Sodium <- "Red"
  clrs$Ammonium <- "Light Sea Green"
  clrs$Potassium <- "Orange Red"
  clrs$Magnesium <- "Purple"
  clrs$Calcium <- "Deep Sky Blue"
  return(clrs)
}
