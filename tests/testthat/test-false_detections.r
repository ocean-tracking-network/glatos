# Testing column that data.frame input returns expected
test_that("data.frame input gives expected result", {
  df_test <- walleye_detections[129:138, ]

  # Check if expected and actual results are the same
  expect_snapshot(
    false_detections(
      df_test,
      3600
    )
  )
})

# Testing column that data.frame input returns expected
test_that("data.table input gives expected result", {
  dt_test <- data.table::as.data.table(walleye_detections[129:138, ])

  # Check if expected and actual results are the same
  expect_snapshot(
    false_detections(
      dt_test,
      3600
    )
  )
})

# Testing column that tibble input returns expected
test_that("tibble input gives expected result", {
  tbl_test <- tibble::as_tibble(walleye_detections[129:138, ])

  # Check if expected and actual results are the same
  expect_snapshot(
    false_detections(
      tbl_test,
      3600
    )
  )
})
