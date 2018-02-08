#' @title Zipped GLATOS detection file from Huron Erie Corridor Walleye project
#' 
#' @description An example detection file
#' 
#' @format A zipped walleye detection file in detection file format 1.3:
##' 
#' @name raw_walleye_detections
#' 
#' @section Filename: walleye_detections.zip
#' 
#' @usage 
#' system.file("extdata", "walleye_detections.zip", package="glatos")
#'
#' @author Todd Hayden
#' 
#' @source \url{http://glatos.glos.us/home/project/HECWL}
NULL


#' @title Raw GLATOS Workbook from St. Marys River Sea Lamprey project
#' 
#' @description A completed GLATOS workbook from St. Marys River Sea Lamprey 
#' project.
#' 
#' @format A macro-enabled Microsoft Excel workbook file (*.xlsm) with six 
#'  worksheets:
#'  \describe{
#'    \item{project}{project code, principal investigator and contact}
#'    \item{locations}{descriptions of receiver array locations}
#'    \item{proposed}{proposed receiver deployment locations and dates}
#'    \item{deployment}{receiver deployment data (what, where, when, how)}
#'    \item{recovery}{receiver recovery data (what, where, when, how)}
#'    \item{tagging}{animal collection, tagging, and recovery data}
#'  }
#' 
#' @name raw_lamprey_workbook
#' 
#' @section Filename: SMRSL_GLATOS_20140828.xlsm
#' 
#' @usage 
#' system.file("extdata", "SMRSL_GLATOS_20140828.xlsm", package="glatos")
#'
#' @author Chris Holbrook
#' 
#' @source \url{http://glatos.glos.us/home/project/SMRSL}
NULL


#' @title Video frames of walleye movements in Lake Huron
#' 
#' @description Sequential images of walleye movements in Lake Huron
#'   for testing functionality of ffmpeg function.
#' 
#' @format Folder contains 30 sequentially labeled .png image files
#' 
#' @name video images
#' 
#' @section Filename: frames
#' 
#' @usage 
#' system.file("extdata", "frames", package="glatos")
#'
#' @author Todd Hayden
#' 
#' @source \url{http://glatos.glos.us/home/project/HECWL}
NULL

#' @title zipped polygon shapefile of Great Lakes
#' 
#' @description Polygon coastline of Great Lakes in WGS84
#'   projection.  Includes outlines of Tittabawassee River (Lake
#'   Huron), Maumee River (Lake Erie), and Sandusky River (Lake Erie)
#' 
#' @format shapefile
#' 
#' @name shoreline
#' 
#' @section Filename: shoreline.zip
#' 
#' @usage 
#' system.file("extdata", "shoreline.zip", package="glatos")
#'
#' @author Todd Hayden
#' 
#' @source \url{http://glatos.glos.us/home}
NULL

