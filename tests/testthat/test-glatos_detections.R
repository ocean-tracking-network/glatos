# R/class-glatos_detections.r

test_that("glatos_detections works as expected", {
  
  # data.frame input
  
  x <- data.frame(
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
    deploy_long = c(-83.99264, -83.99264, -83.99264, -83.99264)
  )
  
  expect_equal(
    gd_df <- glatos_detections(
      animal_id = x$animal_id,
      detection_timestamp_utc =
        x$detection_timestamp_utc,
      deploy_lat = x$deploy_lat,
      deploy_long = x$deploy_long
    ),
    gd_df_shouldbe
  )
  
  expect_equal(
    as_glatos_detections(x),
    gd_df_shouldbe
  )
  
  expect_true(
    validate_glatos_detections(x)
  )
  
  expect_true(
    is_glatos_detections(gd_df) 
  )
  
  expect_false(
    is_glatos_detections(x) 
  )
  
  # sf input
  
  x_sf <- sf::st_as_sf(x,
                       coords = c("deploy_long", "deploy_lat"),
                       remove = FALSE
  )
  
  expect_s3_class(
    gd_sf <- as_glatos_detections(x_sf),
    c("glatos_detections", "sf", "data.frame")
  )
  
  expect_equal(
    sf::st_drop_geometry(gd_sf),
    gd_df_shouldbe
  )
  
  
  # tibble input
  
  x_tbl <- dplyr::as_tibble(x)
  
  expect_s3_class(
    gd_tbl <- as_glatos_detections(x_tbl),
    c("tbl_df", "tbl", "data.frame")
  )
  
  expect_equal(
    as.data.frame(gd_tbl),
    as.data.frame(gd_df_shouldbe)
  )
  
})



test_that("validate_glatos_detections catches bad inputs", {
  
  x <- data.frame(
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
    deploy_long = c(-83.99264, -83.99264, -83.99264, -83.99264)
  )
  
  # data.frame input; missing column name
  expect_error(
    as_glatos_detections(dplyr::rename(x,
                                    fish_name = animal_id
    )
    ),
    regexp = "Required column(s) missing from input x",
    fixed = TRUE
  )
  

  # data.frame input; wrong column class
  expect_error(
    as_glatos_detections(
      plyr::mutate(x,
                   animal_id = as.integer(animal_id)
      )
    ),
    regexp = "The following column(s) have wrong class",
    fixed = TRUE
  )
  

  # non-data.frame input
  expect_error(
    as_glatos_detections(
      unclass(x) 
    ),
    regex = "Input x must inherit from data.frame",
    fixed = TRUE
  )
  
})
