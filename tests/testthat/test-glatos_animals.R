# R/class-glatos_animals.R


test_that("glatos_animals works as expected", {
  # data.frame input

  x <- data.frame(
    animal_id = c("120", "107", "109"),
    tag_id_code = c("32024", "32012", "32014"),
    tag_code_space = c("A69-9001", "A69-9001", "A69-9001"),
    utc_release_date_time = as.POSIXct(
      c(
        "2011-03-28 00:00:00",
        "2011-03-28 00:01:00",
        "2011-03-28 00:05:00"
      ),
      tz = "UTC"
    ),
    release_latitude = c(41.56093, 41.56093, 41.56093),
    release_longitude = c(-83.645, -83.645, -83.645)
  )

  expect_equal(
    ga_df <- glatos_animals(
      animal_id = x$animal_id,
      tag_id_code = x$tag_id_code,
      tag_code_space = x$tag_code_space,
      utc_release_date_time = x$utc_release_date_time,
      release_latitude = x$release_latitude,
      release_longitude = x$release_longitude
    ),
    ga_df_shouldbe
  )

  expect_equal(
    as_glatos_animals(x),
    ga_df_shouldbe
  )

  expect_true(
    validate_glatos_animals(x)
  )

  expect_true(
    is_glatos_animals(ga_df)
  )

  expect_false(
    is_glatos_animals(x)
  )

  # sf input

  x_sf <- sf::st_as_sf(x,
    coords = c("release_longitude", "release_latitude"),
    remove = FALSE
  )

  expect_s3_class(
    ga_sf <- as_glatos_animals(x_sf),
    c("glatos_animals", "sf", "data.frame")
  )

  expect_equal(
    sf::st_drop_geometry(ga_sf),
    ga_df_shouldbe
  )


  # tibble input

  x_tbl <- dplyr::as_tibble(x)

  expect_s3_class(
    ga_tbl <- as_glatos_animals(x_tbl),
    c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    as.data.frame(ga_tbl),
    as.data.frame(ga_df_shouldbe)
  )
})



test_that("validate_glatos_animals catches bad inputs", {
  # req_cols <- list(
  #   animal_id = "character",
  #   tag_id_code = "character",
  #   tag_code_space = "character",
  #   utc_release_date_time = "POSIXct"
  # )

  x <- data.frame(
    animal_id = c("120", "107", "109"),
    tag_id_code = c("32024", "32012", "32014"),
    tag_code_space = c("A69-9001", "A69-9001", "A69-9001"),
    utc_release_date_time = as.POSIXct(
      c(
        "2011-03-28 00:00:00",
        "2011-03-28 00:01:00",
        "2011-03-28 00:05:00"
      ),
      tz = "UTC"
    ),
    release_latitude = c(41.56093, 41.56093, 41.56093),
    release_longitude = c(-83.645, -83.645, -83.645)
  )

  # data.frame input; missing column name
  expect_error(
    as_glatos_animals(dplyr::rename(x,
      fish_name = animal_id,
      release_timestamp = utc_release_date_time
    )),
    regexp = "Required column(s) missing from input x",
    fixed = TRUE
  )

  # # glatos_check_col_names
  # expect_error(
  #   glatos_check_col_names(
  #     dplyr::rename(x,
  #                   fish_name = animal_id,
  #                   release_timestamp = utc_release_date_time
  #     ),
  #     req_cols
  #   ),
  #   regexp = "Required column(s) missing from input x",
  #   fixed = TRUE
  # )


  # data.frame input; wrong column class
  expect_error(
    as_glatos_animals(
      plyr::mutate(x,
        animal_id = as.integer(animal_id),
        utc_release_date_time = as.character(utc_release_date_time)
      )
    ),
    regexp = "The following column(s) have wrong class",
    fixed = TRUE
  )

  # # glatos_check_col_names
  # expect_error(
  #   glatos_check_col_classes(
  #     plyr::mutate(x,
  #                  animal_id = as.integer(animal_id),
  #                  utc_release_date_time = as.character(utc_release_date_time)
  #     ),
  #     req_cols
  #   ),
  #   regexp = "The following column(s) have wrong class",
  #   fixed = TRUE
  # )


  # non-data.frame input
  expect_error(
    as_glatos_animals(
      unclass(x)
    ),
    regex = "Input x must inherit from data.frame",
    fixed = TRUE
  )
})
