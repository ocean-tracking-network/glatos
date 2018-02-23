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
#' This function may be developed in the future to dictate conversion 
#' constuction from a data frame. 
#' 
#' @keywords internal

glatos_workbook <- function(x)  {
  
  #add new class as first but keep existing (e.g., data.frame)
  class(x) <- c("glatos_workbook", class(x))
  
  return (x)
}