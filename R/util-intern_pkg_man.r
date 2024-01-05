# Interal utility functions for package management

#--------------------------------------------------------------------------
# Add an internal data object to sysdata.rda
add_internal_data <- function(x, rda_file) {
  if (file.exists(rda_file)) {
    e <- new.env()
    load(rda_file, e) # load existing internals to new envir
    assign(deparse(substitute(x)), x, envir = e) # add/replace with new object
    save(
      list = ls(e), file = rda_file, envir = e, compress = "bzip2",
      version = 2
    )
  } else {
    usethis::use_data(x, internal = TRUE, version = 2)
  }
}

#--------------------------------------------------------------------------
# Remove an internal data object from sysdata.rda
remove_internal_data <- function(x, rda_file) {
  e <- new.env()
  load(rda_file, e) # load existing internals to new envir
  rm(list = deparse(substitute(x)), envir = e) # remove unwanted object
  save(
    list = ls(e), file = rda_file, envir = e, compress = "bzip2",
    version = 2
  )
}

#--------------------------------------------------------------------------
# Rebuild all internal data objects in sysdata.rda with all objects
# defined in data-raw
rebuild_internal_data <- function(rda_file = "R/sysdata.rda") {
  data_raw_files <- list.files("./data-raw", full.names = TRUE)
  sapply(data_raw_files, source)
  return()
}
