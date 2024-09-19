# Make vemco_tag_spec_schema (internal data object for read_vdat_csv)
# Specify column names and data types for each each Vemco tag spec file version


# Make list element for each workbook version
# pre-allocate sheet-level structure within each version
vemco_tag_spec_schema <- list(
  "vemco_xls" = list(
    "license" = NA,
    "tag summary" = NA
  )
)

vemco_tag_spec_schema$vemco_xls$`tag summary` <- read.table(
  text = '
  name                                  type
  "Sales Order"	                        character
  "Serial No."                          character
  "# of ID\'s"                          numeric
  "Customer"                            character
  "Researcher"                          character
  "Tag Family"                          character
  "VUE Tag ID"                          character
  "Est tag life (days)"	                numeric
  "Step 1 Status"                       character
  "Step 1 Time     (dy hr:min:sec)"	    character
  "Step 1 Power (L/H)"                  character
  "Step 1 Acc. On (sec)"	              numeric
  "Step 1 Min Delay (sec)"              numeric
  "Step 1 Max Delay (sec)"              numeric
  "Step 2 Status"                       character
  "Step 2 Time       (dy hr:min:sec)"	  character
  "Step 2 Power (L/H)"                  character
  "Step 2 Acc. On (sec)"                numeric
  "Step 2 Min Delay (sec)"              numeric
  "Step 2 Max Delay (sec)"              numeric
  "Step 3 Status"                       character
  "Step 3 Time       (dy hr:min:sec)"   character
  "Step 3 Power (L/H)"                  character
  "Step 3 Acc. On (sec)"                numeric
  "Step 3 Min Delay (sec)"              numeric
  "Step 3 Max Delay (sec)"              numeric
  "Step 4 Status"                       character
  "Step 4 Time         (dy hr:min:sec)" character
  "Step 4 Power (L/H)"                  character
  "Step 4 Acc. On (sec)"                numeric
  "Step 4 Min Delay (sec)"              numeric
  "Step 4 Max Delay (sec)"              numeric
  "Loop To"                             character
  "Sensor type"                         character
  "Range"                               character
  "Units"                               character
  "Slope"                               numeric
  "Intercept"                           numeric
  "Accelerometer Algorithm"             character
  "Accelerometer Samples (/sec)"        numeric
  "Sensor Transmit Ratio"               numeric
  "95% Est Battery Life (days)"         numeric
  "50% Est Battery Life (days)"         numeric
  "Ship Date"                           character
  ',
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  comment.char = ""
)

#---------------------------------

# add to sysdata.rda
rda_file <- file.path("R/sysdata.rda")
add_internal_data(vemco_tag_spec_schema, rda_file)
