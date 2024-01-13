# Testing column that data.frame input returns expected
test_that("data.frame input gives expected result", {
  df_in <- walleye_detections[129:138,]

  expect_message(
    df_result <- false_detections(
      df_in,
      3600
    ),
    'The filter identified 2 \\(20\\%\\) of 10 detections as potentially false\\.'
  )

  expect_s3_class(df_result, 'glatos_detections')
  expect_s3_class(df_result, 'data.frame')

  expect_equal(dim(df_result), c(10, 31))

  expect_type(df_result$passed_filter, 'double')
  expect_equal(df_result$passed_filter,
               c(1, 1, 0, 1, 1, 1, 1, 0 ,1, 1))

  expect_equal(df_result[, 1:(ncol(df_result) - 1)],
               df_in)

  # Check if expected and actual results are the same
  expect_snapshot(
    df_result
  )
})

# Testing column that data.frame input returns expected
test_that("data.table input gives expected result", {

  dt_in <- data.table::as.data.table(walleye_detections[129:138,])

  expect_message(
    dt_result <- false_detections(
      dt_in,
      3600
    ),
    'The filter identified 2 \\(20\\%\\) of 10 detections as potentially false\\.'
  )

  expect_s3_class(dt_result, 'data.table')
  expect_s3_class(dt_result, 'data.frame')

  expect_equal(dim(dt_result), c(10, 31))

  expect_type(dt_result$passed_filter, 'double')
  expect_equal(dt_result$passed_filter,
               c(1, 1, 0, 1, 1, 1, 1, 0 ,1, 1))

  expect_equal(dt_result[, 1:(ncol(dt_result) - 1)],
               dt_in)

  # Check if expected and actual results are the same
  expect_snapshot(
    dt_result
  )
})

# Testing column that tibble input returns expected
test_that("tibble input gives expected result", {
  tbl_in <- tibble::as_tibble(walleye_detections[129:138,])

  expect_message(
    tbl_result <- false_detections(
      tbl_in,
      3600
    ),
    'The filter identified 2 \\(20\\%\\) of 10 detections as potentially false\\.'
  )

  expect_s3_class(tbl_result, 'tbl_df')
  expect_s3_class(tbl_result, 'tbl')
  expect_s3_class(tbl_result, 'data.frame')

  expect_equal(dim(tbl_result), c(10, 31))

  expect_type(tbl_result$passed_filter, 'double')
  expect_equal(tbl_result$passed_filter,
               c(1, 1, 0, 1, 1, 1, 1, 0 ,1, 1))

  expect_equal(tbl_result[, 1:(ncol(tbl_result) - 1)],
               tbl_in)

  # Check if expected and actual results are the same
  expect_snapshot(
    tbl_result
  )
})
