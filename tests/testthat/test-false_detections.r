context("Check false_detections")

# Sample data for min_lag
det_df_exp <- structure(list(
  animal_id = c(
    "153", "153", "153", "153", "153",
    "153", "153", "153", "153", "153"
  ), detection_timestamp_utc = structure(c(
    1337741659,
    1337743033, 1337743278, 1337743396, 1337743572, 1337744344, 1337744491,
    1337744491, 1337744686, 1337745421
  ), class = c("POSIXct", "POSIXt"), tzone = "UTC"), glatos_array = c(
    "SBI", "SBI", "SBI", "SBI",
    "SBI", "SBI", "SBI", "SBI", "SBI", "SBI"
  ), station_no = c(
    "1",
    "1", "3", "1", "1", "1", "1", "2", "1", "1"
  ), transmitter_codespace = c(
    "A69-9001",
    "A69-9001", "A69-9001", "A69-9001", "A69-9001", "A69-9001", "A69-9001",
    "A69-9001", "A69-9001", "A69-9001"
  ), transmitter_id = c(
    "32054",
    "32054", "32054", "32054", "32054", "32054", "32054", "32054",
    "32054", "32054"
  ), sensor_value = c(
    NA_real_, NA_real_, NA_real_,
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
  ), sensor_unit = c(
    NA_character_, NA_character_, NA_character_,
    NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
    NA_character_, NA_character_
  ), deploy_lat = c(
    44.17873, 44.17873,
    44.17255, 44.17873, 44.17873, 44.17873, 44.17873, 44.17714, 44.17873,
    44.17873
  ), deploy_long = c(
    -83.54767, -83.54767, -83.5309, -83.54767,
    -83.54767, -83.54767, -83.54767, -83.54169, -83.54767, -83.54767
  ), receiver_sn = c(
    "109991", "109991", "109999", "109991", "109991",
    "109991", "109991", "109956", "109991", "109991"
  ), tag_type = c(
    NA_character_,
    NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
    NA_character_, NA_character_, NA_character_, NA_character_
  ),
  tag_model = c(
    NA_character_, NA_character_, NA_character_,
    NA_character_, NA_character_, NA_character_, NA_character_,
    NA_character_, NA_character_, NA_character_
  ), tag_serial_number = c(
    NA_character_,
    NA_character_, NA_character_, NA_character_, NA_character_,
    NA_character_, NA_character_, NA_character_, NA_character_,
    NA_character_
  ), common_name_e = c(
    "walleye", "walleye", "walleye",
    "walleye", "walleye", "walleye", "walleye", "walleye", "walleye",
    "walleye"
  ), capture_location = c(
    "Tittabawassee River", "Tittabawassee River",
    "Tittabawassee River", "Tittabawassee River", "Tittabawassee River",
    "Tittabawassee River", "Tittabawassee River", "Tittabawassee River",
    "Tittabawassee River", "Tittabawassee River"
  ), length = c(
    0.565,
    0.565, 0.565, 0.565, 0.565, 0.565, 0.565, 0.565, 0.565, 0.565
  ), weight = c(
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
  ), sex = c(
    "F",
    "F", "F", "F", "F", "F", "F", "F", "F", "F"
  ), release_group = c(
    NA_character_,
    NA_character_, NA_character_, NA_character_, NA_character_,
    NA_character_, NA_character_, NA_character_, NA_character_,
    NA_character_
  ), release_location = c(
    "Tittabawassee", "Tittabawassee",
    "Tittabawassee", "Tittabawassee", "Tittabawassee", "Tittabawassee",
    "Tittabawassee", "Tittabawassee", "Tittabawassee", "Tittabawassee"
  ), release_latitude = c(
    NA_real_, NA_real_, NA_real_, NA_real_,
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
  ), release_longitude = c(
    NA_real_, NA_real_, NA_real_, NA_real_,
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
  ), utc_release_date_time = structure(c(
    1332273600, 1332273600,
    1332273600, 1332273600, 1332273600, 1332273600, 1332273600,
    1332273600, 1332273600, 1332273600
  ), class = c(
    "POSIXct",
    "POSIXt"
  ), tzone = "UTC"), glatos_project_transmitter = c(
    "HECWL",
    "HECWL", "HECWL", "HECWL", "HECWL", "HECWL", "HECWL", "HECWL",
    "HECWL", "HECWL"
  ), glatos_project_receiver = c(
    "HECWL", "HECWL",
    "HECWL", "HECWL", "HECWL", "HECWL", "HECWL", "HECWL", "HECWL",
    "HECWL"
  ), glatos_tag_recovered = c(
    "NO", "NO", "NO", "NO",
    "NO", "NO", "NO", "NO", "NO", "NO"
  ), glatos_caught_date = structure(c(
    NA_real_,
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
    NA_real_, NA_real_, NA_real_
  ), class = "Date"), station = c(
    "SBI-001",
    "SBI-001", "SBI-003", "SBI-001", "SBI-001", "SBI-001", "SBI-001",
    "SBI-002", "SBI-001", "SBI-001"
  ), min_lag = c(
    1374, 363,
    4180, 176, 176, 147, 147, 4478, 195, 735
  ), passed_filter = c(
    1,
    1, 0, 1, 1, 1, 1, 0, 1, 1
  )
), row.names = 129:138, class = c(
  "glatos_detections",
  "data.frame"
))


# strip last col to create input

# data.frame
det_df_in <- det_df_exp[, 1:(ncol(det_df_exp) - 1)]


# data.table
det_dt_in <- data.table::as.data.table(det_df_in)

# tibble
det_tbl_in <- tibble::as_tibble(det_df_in)

# results
det_df_out <- false_detections(det_df_in, 3600)
det_dt_out <- false_detections(det_dt_in, 3600)
det_tbl_out <- false_detections(det_tbl_in, 3600)


# Testing column that data.frame input returns expected
test_that("data.frame input gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(det_df_out, det_df_exp)
})

# Testing column that data.frame input returns expected
test_that("data.table input gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(det_dt_out, data.table::as.data.table(det_df_exp))
})

# Testing column that tibble input returns expected
test_that("tibble input gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(det_tbl_out, tibble::as_tibble(det_df_exp))
})
