context("Check convert_otn_erddap_to_att")

# check against internal data object 'blue_shark_att' in R/sysdata.r

# Actual result
# get blue shark example data
dtc_file <- system.file("extdata",
  "blue_shark_detections.csv",
  package = "glatos"
)
shrk_det_file <- system.file("extdata", "blue_shark_detections.csv",
  package = "glatos"
)
blue_shark_detections <- read_otn_detections(shrk_det_file)


# get path to example files from OTN ERDDAP
ani_erd_file <- system.file("extdata", "otn_aat_animals.csv",
  package = "glatos"
)
animals <- read.csv(ani_erd_file, as.is = TRUE) # load the CSVs from ERDDAP

tags_erd_file <- system.file("extdata", "otn_aat_tag_releases.csv",
  package = "glatos"
)
tags <- read.csv(tags_erd_file, as.is = TRUE)

rcv_erd_file <- system.file("extdata", "otn_aat_receivers.csv",
  package = "glatos"
)
stations <- read.csv(rcv_erd_file, as.is = TRUE)

# Remove first row; (blank or metadata about the column)
animals <- animals[-1, ]
tags <- tags[-1, ]
stations <- stations[-1, ]

# create ATT object
bs_att <- convert_otn_erddap_to_att(
  blue_shark_detections,
  tags, stations, animals
)



# Test using testthat library
test_that("blue_shark_erddap_att gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(bs_att, blue_shark_erddap_att)
})
