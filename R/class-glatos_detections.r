#' Constructor function for the class glatos_detections
#' 
#' @description
#' Constructor function for the class glatos_detections. Currently barebones 
#' and only used inside read_glatos_detections and read_otn_detections.
#'
#' @param x A data.frame or data.table created from a standard glatos 
#' detection file.
#' 
#' @return A data.frame of class \code{glatos_detections}:
#'
#' @note
#' This function may be developed in the future to dictate conversion 
#' constuction from a data frame.  

glatos_detections <- function(x)  {

  #coerce to data.frame if not
  if(inherits(x, "data.table")) x <- as.data.frame(x)
  if(!inherits(x, "data.frame")) stop("x must be data.frame")
  
  #add new class as first but keep existing (e.g., data.frame)
  class(x) <- c("glatos_detections", class(x))
  
  return (x)
}