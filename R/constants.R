# Constants

#' Function that defines plotting colours for 

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
