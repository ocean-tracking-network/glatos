#Interal utility functions for package management

#--------------------------------------------------------------------------
#Add an internal data object to sysdata.rda
add_internal_data <- function(x, rda_file) {
  e <- new.env()
  load(rda_file, e) #load existing internals to new envir
  assign(deparse(substitute(x)), x, envir = e) #add/replace with new object
  save(list = ls(e), file = rda_file, envir = e, version = 2)
}

#--------------------------------------------------------------------------
#Remove an internal data object from sysdata.rda
remove_internal_data <- function(x, rda_file) {
  e <- new.env()
  load(rda_file, e) #load existing internals to new envir
  rm(list = deparse(substitute(x)), envir = e) #remove unwanted object
  save(list = ls(e), file = rda_file, envir = e, version = 2)
}
