# R/proc-real_sensor_values.r

test_that("real_sensor_values works", {
  # get unique tag code
  ids <- unique(lamprey_tag_specs$specs$id_code)

  # subset just first two detections of each tag in ids
  dtc <- as.data.frame(
    data.table::as.data.table(lamprey_detections)[
      transmitter_id %in% ids,
      .SD[1:2],
      by = transmitter_id
    ]
  )

  expect_snapshot(
    real_sensor_values(dtc, lamprey_tag_specs$specs)
  )
})
