#' Constructor function for the class glatos_receiver_locations
#' 
#' @description Constructor function for the class glatos_receiver_locations.
#' Currently barebones and only used inside read_glatos_receiver_locations.
#'
#' @param x A data.frame or data.table created from a standard glatos 
#' receiver_location file.
#' 
#' @return A data.frame of class \code{glatos_receiver_locations}:
#'
#' @note
#' This function may be expanded in the future to dictate conversion 
#' between various data sources (glatos, otn, other) and versions to 
#' a single 'glatos_detection' standard format. One way to do this might
#' be to (1) specify name_in, name_out, and type_in, type_out eg in the 
#' schema data files, (2) use "..." argument to this constructor function to
#' allow each standard column to be mapped directly from source, and (3) 
#' check that requirements are met. 

glatos_receiver_locations <- function(x)  {

  #coerce to data.frame if not
  if(inherits(x, "data.table")) x <- as.data.frame(x)
  if(!inherits(x, "data.frame")) stop("x must be data.frame")
  
  #add new class as first but keep existing (e.g., data.frame)
  class(x) <- c("glatos_receiver_locations", class(x))
  
  return (x)
}