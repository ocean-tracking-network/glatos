test_that("summarize_detections works", {
  # summ_type = animal
  expect_snapshot(
    summarize_detections(walleye_detections)
  )

  # summ_type = location
  expect_snapshot(
    summarize_detections(walleye_detections, summ_type = "location")
  )

  # summ_type = both
  expect_snapshot(
    summarize_detections(walleye_detections, summ_type = "both")
  )

  # summ_type = location; with receiver_locs
  expect_snapshot(
    summarize_detections(
      walleye_detections,
      receiver_locs = sample_receivers,
      summ_type = "location"
    )
  )

  # summ_type = both; with receiver_locs and animals
  expect_snapshot(
    summarize_detections(
      walleye_detections,
      receiver_locs = sample_receivers,
      animals = walleye_workbook$animals,
      summ_type = "both"
    )
  )
})


test_that("bad inputs are caught", {
  # summ_type = animal
  expect_error(
    summarize_detections(walleye_detections, summ_type = "foo")
  )

  # missing columns in det
  expect_error(
    summarize_detections(walleye_detections[, -1])
  )

  # missing 'location_col' in det
  expect_error(
    summarize_detections(walleye_detections[, -3])
  )

  # detection_timestamp_utc not POSIXct in det
  expect_error(
    summarize_detections(
      data.table::as.data.table(walleye_detections)[
        ,
        detection_timestamp_utc := as.numeric(detection_timestamp_utc)
      ]
    )
  )

  # missing 'location_col' in receiver locs
  expect_error(
    summarize_detections(
      walleye_detections,
      receiver_locs = sample_receivers[, -2]
    )
  )
})


test_that("output class matches input class", {
  # data.frame input
  expect_s3_class(
    summarize_detections(walleye_detections),
    "data.frame"
  )

  # data.table input
  expect_s3_class(
    summarize_detections(data.table::as.data.table(walleye_detections)),
    "data.table"
  )

  # tibble input
  expect_s3_class(
    summarize_detections(dplyr::as_tibble(walleye_detections)),
    "tbl"
  )
})
