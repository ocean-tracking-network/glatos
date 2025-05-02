# check against internal data object 'blue_shark_att' in R/sysdata.r

# Actual result
# get blue shark example data
dtc_file <- system.file(
  "extdata",
  "blue_shark_detections.csv",
  package = "glatos"
)
shrk_det_file <- system.file(
  "extdata",
  "blue_shark_detections.csv",
  package = "glatos"
)
blue_shark_detections <- read_otn_detections(shrk_det_file)


# get path to example files from OTN ERDDAP
ani_erd_file <- system.file(
  "extdata",
  "otn_aat_animals.csv",
  package = "glatos"
)
animals <- read.csv(ani_erd_file, as.is = TRUE) # load the CSVs from ERDDAP

tags_erd_file <- system.file(
  "extdata",
  "otn_aat_tag_releases.csv",
  package = "glatos"
)
tags <- read.csv(tags_erd_file, as.is = TRUE)

rcv_erd_file <- system.file(
  "extdata",
  "otn_aat_receivers.csv",
  package = "glatos"
)
stations <- read.csv(rcv_erd_file, as.is = TRUE)

# Remove first row; (blank or metadata about the column)
animals <- animals[-1, ]
tags <- tags[-1, ]
stations <- stations[-1, ]


# Test using testthat library
test_that("matches internal data: blue_shark_erddap_att", {
  # create ATT object
  expect_no_error(
    bs_att <- convert_otn_erddap_to_att(
      blue_shark_detections,
      tags,
      stations,
      animals
    )
  )

  # expect_message(
  #   convert_otn_erddap_to_att(
  #     blue_shark_detections,
  #     tags, stations, animals
  #   )
  # )

  # expect_output(
  #   convert_otn_erddap_to_att(
  #     blue_shark_detections,
  #     tags, stations, animals
  #   )
  # )

  # Check if expected and actual results are the same
  expect_identical(bs_att$Tag.Detections, blue_shark_erddap_att$Tag.Detections)
  expect_identical(bs_att$Tag.Metadata, blue_shark_erddap_att$Tag.Metadata)
  expect_identical(
    bs_att$Station.Information,
    blue_shark_erddap_att$Station.Information
  )
  expect_identical(
    attr(bs_att, "CRS")$epsg,
    attr(blue_shark_erddap_att, "CRS")$epsg
  )
})


test_that("matches type/class of internal data: blue_shark_erddap_att", {
  bs_att <- convert_otn_erddap_to_att(
    blue_shark_detections,
    tags,
    stations,
    animals
  )

  expect_s3_class(bs_att, "ATT")
  expect_type(bs_att, "list")
  expect_named(attributes(bs_att), c("names", "class", "CRS"))
})


# Test non-exported concat_list_strings function
test_that("internal function concat_list_strings works", {
  expect_no_error(
    concat_list_strings(
      blue_shark_detections$transmitter_codespace,
      blue_shark_detections$transmitter_id
    )
  )
})

test_that("internal function concat_list_strings errors with unequal length", {
  expect_error(
    concat_list_strings(
      blue_shark_detections$transmitter_codespace[1:10],
      blue_shark_detections$transmitter_id
    ),
    "Lists are not the same size."
  )
})
