# input data.frame for testing
input <- data.frame(
  animal_id = c("153", "153", "153", "153"),
  detection_timestamp_utc = as.POSIXct(
    c(
      "2012-04-29 01:48:37",
      "2012-04-29 01:52:55",
      "2012-04-29 01:55:12",
      "2012-04-29 01:56:42"
    ),
    tz = "UTC"
  ),
  deploy_lat = c(43.39165, 43.39165, 43.39165, 43.39165),
  deploy_long = c(-83.99264, -83.99264, -83.99264, -83.99264),
  glatos_array = c("one", "one", "two", "two")
)

# test that function returns the correct result
test_that("detection_events returns expected result", {
  expected <- expected_dtc_evts()

  actual <- detection_events(
    det = input,
    location_col = "glatos_array",
    time_sep = Inf,
    condense = TRUE
  )

  expect_equal(as.data.frame(actual), as.data.frame(expected))
})


# no errors, should work.
test_that("validate detection_events catches bad inputs", {
  # should work
  expect_no_error(detection_events(
    input,
    location_col = "glatos_array",
    time_sep = Inf,
    condense = TRUE
  ))

  # function should catch numeric location col argument
  expect_error(
    detection_events(input, location_col = 1, time_sep = Inf, condense = TRUE),
    regexp = "input argument 'location_col' must be a character.",
    fixed = TRUE
  )

  # location col should be in the detections dataframe
  expect_error(
    detection_events(
      input,
      location_col = "wrong",
      time_sep = Inf,
      condense = TRUE
    ),
    regexp = "input argument 'location_col' is not in the input data",
    fixed = TRUE
  )

  # function should catch character instead of logical for condense argument
  expect_error(
    detection_events(
      input,
      location_col = "glatos_array",
      time_sep = Inf,
      condense = "yes"
    ),
    regexp = "input argument 'condense' must be either TRUE or FALSE (logical).",
    fixed = TRUE
  )

  # function should catch length 2 vector input for location_col
  expect_error(
    detection_events(
      input,
      location_col = c("glatos_array", "test"),
      time_sep = Inf,
      condense = TRUE
    ),
    regexp = "input argument 'location_col' must be a single value (length = 1).",
    fixed = TRUE
  )
})
