# input data.frame for testing
# this is the "glatos-detections" output
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

# test that function returns the correct result (condensed output)
test_that("detection_events returns expected result- condensed output", {
  expected <- expected_dtc_evts()

  actual <- detection_events(
    det = input,
    location_col = "glatos_array",
    time_sep = Inf,
    condense = TRUE
  )

  expect_equal(as.data.frame(actual), as.data.frame(expected))
})

# test that function returns the correct results (long format (condense = FALSE)
test_that("detection_events returns expected result- long format output", {
  expected <- expected_dtc_events_long()

  actual <- detection_events(
    det = input,
    location_col = "glatos_array",
    time_sep = Inf,
    condense = FALSE
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

  # function should generate a warning if it automatically converts time_sep to numeric value
  expect_warning(
    detection_events(
      input,
      location_col = "glatos_array",
      time_sep = "3600",
      condense = TRUE
    ),
    regexp = "Supplied `time_sep` argument was not numeric.  Attempted conversion to numeric value.",
    fixed = TRUE
  )

  # function should generate an error if it time_sep is not a single numeric scalar greater that 0 and not NA
  # test time_sep = NA (missing value)
  expect_error(
    detection_events(
      input,
      location_col = "glatos_array",
      time_sep = NA,
      condense = TRUE
    ),
    regexp = "Input argument 'time_sep' must be numeric, a single scaler (length = 1), and greater than 0.",
    fixed = TRUE
  )

  #test time_sep = -1000 (negative value)
  expect_error(
    detection_events(
      input,
      location_col = "glatos_array",
      time_sep = -1000,
      condense = TRUE
    ),
    regexp = "Input argument 'time_sep' must be numeric, a single scaler (length = 1), and greater than 0.",
    fixed = TRUE
  )

  #test time_sep = c(1000, 2000) (multiple values)
  expect_error(
    detection_events(
      input,
      location_col = "glatos_array",
      time_sep = c(1000, 2000),
      condense = TRUE
    ),
    regexp = "Input argument 'time_sep' must be numeric, a single scaler (length = 1), and greater than 0.",
    fixed = TRUE
  )

  # function should catch that location col is not in the detections dataframe
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
    regexp = "Input argument 'condense' must be either TRUE or FALSE (logical).",
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
