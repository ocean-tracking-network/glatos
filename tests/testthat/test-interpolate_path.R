# Set up data from interpolate_path example 1
pos <- data.frame(
  animal_id=1,
  deploy_long=c(-87,-82.5, -78),
  deploy_lat=c(44, 44.5, 43.5),
  detection_timestamp_utc=as.POSIXct(
    c("2000-01-01 00:00", "2000-02-01 00:00", "2000-03-01 00:00"),
    tz = "UTC"
  )
)




test_that("linear interpolation works", {
  linear_interp <- interpolate_path(pos)
  expect_snapshot(
    linear_interp
  )
  
  expect_s3_class(
    linear_interp,
    'data.frame'
  )
  
  expect_named(
    linear_interp,
    c("animal_id", "bin_timestamp", "latitude", "longitude", "record_type")
  )
  
  # Retains actual detections
  expect_equal(
    nrow(
      linear_interp[linear_interp$record_type == 'detection',]
    ),
    nrow(pos)
  )
})




test_that("Non-linear interpolation works", {
  linear_interp <- interpolate_path(pos)
  expect_message(
    nonlinear_interp <- interpolate_path(pos, trans = greatLakesTrLayer),
    'Calculating least-cost \\(non-linear\\) distances\\.\\.\\. \\(step 1 of 3\\)'
  ) |> 
    expect_output('|=*| 100%') |> 
    expect_message('Starting non-linear interpolation\\.\\.\\. \\(step 3 of 3\\)') |> 
    expect_message('Finalizing results\\.') 
  
  expect_false(
    identical(
      linear_interp,
      nonlinear_interp
    )
  )
  
  expect_snapshot(
    nonlinear_interp
  )
  
  expect_s3_class(
    nonlinear_interp,
    'data.frame'
  )
  
  expect_named(
    nonlinear_interp,
    c("animal_id", "bin_timestamp", "latitude", "longitude", "record_type")
  )
  
  # Retains actual detections
  expect_equal(
    nrow(
      nonlinear_interp[nonlinear_interp$record_type == 'detection',]
    ),
    nrow(pos)
  )
})



test_that("Forced linear interpolation works", {
  linear_interp <- interpolate_path(pos)
  nonlinear_interp <- suppressMessages(
    interpolate_path(pos, trans = greatLakesTrLayer,
                     show_progress = FALSE)
  )
  
  
  forced_linear_interp <- interpolate_path(pos, trans = greatLakesTrLayer,
                                           lnl_thresh = 0) |> 
    expect_no_message()
  
  
  expect_false(
    identical(
      forced_linear_interp,
      nonlinear_interp
    )
  )
  
  expect_identical(
    linear_interp,
    forced_linear_interp
  )
  
  expect_s3_class(
    forced_linear_interp,
    'data.frame'
  )
  
  expect_named(
    forced_linear_interp,
    c("animal_id", "bin_timestamp", "latitude", "longitude", "record_type")
  )
  
  # Retains actual detections
  expect_equal(
    nrow(
      forced_linear_interp[forced_linear_interp$record_type == 'detection',]
    ),
    nrow(pos)
  )
})



test_that("Progress bar can be suppressed", {
  expect_output(
    suppressMessages(
      interpolate_path(pos, trans = greatLakesTrLayer,
                       show_progress = FALSE)
    ),
    NA
  )
})




test_that("Checks output class", {
  # Error if wrong class
  expect_error(
    interpolate_path(pos, out_class = 'list'),
    "out_class is not a \"data\\.table\" or \"tibble\""
  )
  
  # data.frame if NULL
  expect_s3_class(
    interpolate_path(pos, out_class = NULL),
    'data.frame',
    exact = TRUE
  )
  
  # data.table
  expect_s3_class(
    interpolate_path(pos, out_class = 'data.table'),
    c('data.table', 'data.frame'),
    exact = TRUE
  )
  
  # tibble
  expect_s3_class(
    interpolate_path(pos, out_class = 'tibble'),
    c('tbl_df', 'tbl', 'data.frame'),
    exact = TRUE
  )
})



test_that("Checks trans is a transition layer or transition stack", {
  suppressMessages(
    trans <- make_transition3(great_lakes_polygon)
  )
  
  expect_error(
    interpolate_path(pos, trans = trans),
    "Supplied object for 'trans' argument is not class TransitionLayer or TransitionStack\\."
  )
})



test_that("Checks that start_time was successfully coverted", {
  expect_error(
    interpolate_path(pos, start_time = NA),
    "start_time cannot be coerced to 'POSIXct' or 'POSIXt' class"
  )
})



test_that("Checks that start_time < largest timestamp in dataset", {
  expect_error(
    interpolate_path(pos, start_time = max(pos$detection_timestamp_utc) + 1),
    "start_time is larger than last detection\\.  No data to interpolate\\!"
  )
})



test_that("Checks that fish have multiple observations", {
  expect_error(
    interpolate_path(pos[1,]),
    "must have two observations to interpolate"
  )
})



test_that("Errors and displays offending receivers if any receivers are on land", {
  pos2 <- pos
  pos2$deploy_long[1] <- -90
  
  expect_error(
    suppressMessages(
      interpolate_path(pos2, trans = greatLakesTrLayer, show_progress = FALSE)
    ),
    "Some coordinates are on land or beyond extent.
    Interpolation impossible! Check receiver locations or extents of transition
    layer:\n"
  )
})
