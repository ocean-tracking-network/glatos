context("Check vrl2csv")


## Access internal VRL
myVRL <- system.file("extdata", "detection_files_raw",
  "VR2W_109924_20110718_1.vrl",
  package = "glatos"
)

## Create temp_dir with spaces in the file path
test_dir <- file.path(tempdir(), "test")
if (!dir.exists(test_dir)) dir.create(test_dir)


## Copy internal VRL to test_dir
good_vrl <- file.path(test_dir, basename(myVRL))
file.copy(myVRL, good_vrl)

## Run vrl2csv
good_csv <- vrl2csv(good_vrl,
  outDir = test_dir,
  vueExePath = "C:/Program Files (x86)/VEMCO/VUE"
)

## Get first 10 lines
csv_f10 <- readLines(good_csv, n = 10)

csv_f10_shouldBe <-
  c(
    "Date and Time (UTC),Receiver,Transmitter,Transmitter Name,Transmitter Serial,Sensor Value,Sensor Unit,Station Name,Latitude,Longitude,Transmitter Type,Sensor Precision",
    "2011-04-11 20:17:49,VR2W-109924,A69-1303-63366,,,,,,+0,+0",
    "2011-05-08 05:38:32,VR2W-109924,A69-9002-4043,,,5,ADC,,+0,+0",
    "2011-05-08 05:41:09,VR2W-109924,A69-9002-4043,,,7,ADC,,+0,+0",
    "2011-05-08 05:43:14,VR2W-109924,A69-9002-4043,,,4,ADC,,+0,+0",
    "2011-05-08 05:44:15,VR2W-109924,A69-9002-4043,,,5,ADC,,+0,+0",
    "2011-05-08 05:45:59,VR2W-109924,A69-9002-4043,,,16,ADC,,+0,+0",
    "2011-05-08 05:46:36,VR2W-109924,A69-9002-4043,,,5,ADC,,+0,+0",
    "2011-05-08 05:48:07,VR2W-109924,A69-9002-4043,,,6,ADC,,+0,+0",
    "2011-05-08 05:48:31,VR2W-109924,A69-9002-4043,,,4,ADC,,+0,+0"
  )

## Delete the CSV that was just made
file.remove(good_csv)


# Check csv from one VRL in dir with space in name
test_that("one vrl in dir with space in name gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(csv_f10, csv_f10_shouldBe)
})



## Make an extra VRL to show behavior if one of group of VRLs is not encoded
corrupt_vrl <- file.path(test_dir, "corrupt.vrl")
file.copy(myVRL, corrupt_vrl)

## Corrupt one of the VRLs
write(c("SOMEgibberish"), corrupt_vrl)

## Try to import
msg <- tryCatch(
  vrl2csv(c(corrupt_vrl, good_vrl),
    outDir = test_dir,
    vueExePath = "C:/Program Files (x86)/VEMCO/VUE"
  ),
  warning = function(w) w$message
)

## Get first 10 lines
csv2_f10 <- readLines(good_csv, n = 10)


# Delete temp_dir
unlink(test_dir, recursive = TRUE)


# Check csv from batch with corrupted VRL in dir with space in name
test_that("one good vrl in dir with corrupt vrl gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(csv2_f10, csv_f10_shouldBe)
})

# Check warning msg when corrupted vrl

msg_shouldBe <- "corrupt.csv was not created."

test_that("warning with corrupt vrl gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(msg, msg_shouldBe)
})
