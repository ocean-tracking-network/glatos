#' Constructor function for the class glatos_workbook
#' 
#' @description Constructor function for the class glatos_workbook.
#' Currently barebones and only used inside read_glatos_workbook.
#'
#' @param x A list containing data from a standard GLATOS data workbook 
#' (*.xlsm) file.
#' 
#' @return A list of class \code{glatos_workbook} created from a standard GLATOS
#'   data workbook (*.xlsm) file with three elements:
#' \describe{
#'   \item{metadata}{A list with data about the project.}
#'   \item{animals}{A data frame with data about tagged animals.}
#'   \item{receivers}{A data frame with data about receivers.}
#' }
#'
#' @note
#' This function may be expanded in the future to dictate conversion 
#' between various data sources (glatos, otn, other) and versions to 
#' a single 'glatos_workbook' standard format. One way to do this might
#' be to (1) specify name_in, name_out, and type_in, type_out eg in the 
#' schema data files, (2) use "..." argument to this constructor function to
#' allow each standard column to be mapped directly from source, and (3) 
#' check that requirements are met. 

glatos_workbook <- function(x)  {
  
  #add new class as first but keep existing (e.g., data.frame)
  class(x) <- c("glatos_workbook", class(x))
  
  return (x)
}