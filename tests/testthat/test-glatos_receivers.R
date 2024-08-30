# R/class-glatos_receivers.r

test_that("glatos_receivers works as expected", {
  # data.frame input

  x <- data.frame(
    station = c("WHT-009", "FDT-001", "FDT-004", "FDT-003"),
    deploy_lat = c(43.7, 45.9, 45.9, 45.9),
    deploy_long = c(-82.5, -83.5, -83.5, -83.5),
    deploy_date_time = as.POSIXct(
      c(
        "2010-09-22 18:05:00",
        "2010-11-12 15:07:00",
        "2010-11-12 15:36:00",
        "2010-11-12 15:56:00"
      ),
      tz = "UTC"
    ),
    recover_date_time = as.POSIXct(
      c(
        "2012-08-15 16:52:00",
        "2012-05-15 13:25:00",
        "2012-05-15 14:15:00",
        "2012-05-15 14:40:00"
      ),
      tz = "UTC"
    ),
    ins_serial_no = c("109450", "442", "441", "444")
  )

  expect_equal(
    gr_df <- glatos_receivers(
      station = x$station,
      deploy_lat = x$deploy_lat,
      deploy_long = x$deploy_long,
      deploy_date_time = x$deploy_date_time,
      recover_date_time = x$recover_date_time,
      ins_serial_no = x$ins_serial_no
    ),
    gr_df_shouldbe
  )

  expect_equal(
    as_glatos_receivers(x),
    gr_df_shouldbe
  )

  expect_true(
    validate_glatos_receivers(x)
  )

  expect_true(
    is_glatos_receivers(gr_df)
  )

  expect_false(
    is_glatos_receivers(x)
  )

  # sf input

  x_sf <- sf::st_as_sf(x,
    coords = c("deploy_long", "deploy_lat"),
    remove = FALSE
  )

  expect_s3_class(
    gr_sf <- as_glatos_receivers(x_sf),
    c("glatos_receivers", "sf", "data.frame")
  )

  expect_equal(
    sf::st_drop_geometry(gr_sf),
    gr_df_shouldbe
  )


  # tibble input

  x_tbl <- dplyr::as_tibble(x)

  expect_s3_class(
    gr_tbl <- as_glatos_receivers(x_tbl),
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    as.data.frame(gr_tbl),
    as.data.frame(gr_df_shouldbe)
  )
})



test_that("validate_glatos_detections catches bad inputs", {
  x <- data.frame(
    station = c("WHT-009", "FDT-001", "FDT-004", "FDT-003"),
    deploy_lat = c(43.7, 45.9, 45.9, 45.9),
    deploy_long = c(-82.5, -83.5, -83.5, -83.5),
    deploy_date_time = as.POSIXct(
      c(
        "2010-09-22 18:05:00",
        "2010-11-12 15:07:00",
        "2010-11-12 15:36:00",
        "2010-11-12 15:56:00"
      ),
      tz = "UTC"
    ),
    recover_date_time = as.POSIXct(
      c(
        "2012-08-15 16:52:00",
        "2012-05-15 13:25:00",
        "2012-05-15 14:15:00",
        "2012-05-15 14:40:00"
      ),
      tz = "UTC"
    ),
    ins_serial_no = c("109450", "442", "441", "444")
  )

  expect_equal(
    gr_df <- glatos_receivers(
      station = x$station,
      deploy_lat = x$deploy_lat,
      deploy_long = x$deploy_long,
      deploy_date_time = x$deploy_date_time,
      recover_date_time = x$recover_date_time,
      ins_serial_no = x$ins_serial_no
    ),
    gr_df_shouldbe
  )

  # data.frame input; missing column name
  expect_error(
    as_glatos_receivers(dplyr::rename(x,
      receiver_id = ins_serial_no
    )),
    regexp = "Required column(s) missing from input x",
    fixed = TRUE
  )


  # data.frame input; wrong column class
  expect_error(
    as_glatos_receivers(
      plyr::mutate(x,
        ins_serial_no = as.integer(ins_serial_no)
      )
    ),
    regexp = "The following column(s) have wrong class",
    fixed = TRUE
  )


  # non-data.frame input
  expect_error(
    as_glatos_receivers(
      unclass(x)
    ),
    regex = "Input x must inherit from data.frame",
    fixed = TRUE
  )
})
