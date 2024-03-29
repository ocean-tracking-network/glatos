context("Check position_heat_map")

# example file from VEMCO VPS
data(lamprey_tracks)
phm_full_input <- position_heat_map(lamprey_tracks, 
                         x_limits = c(-84.14, -84.12),
                         y_limits = c(46.45, 46.47),
                         resolution = 100)

                         
phm_reduced_input <- position_heat_map(lamprey_tracks[, c("DETECTEDID",
                                                 "DATETIME",
                                                 "LAT",
                                                 "LON")],
                         x_limits = c(-84.14, -84.12),
                         y_limits = c(46.45, 46.47),                         
                         resolution = 100)

# Test data.table input
phm_dt_input <- position_heat_map(data.table::setDT(lamprey_tracks), 
                                    x_limits = c(-84.14, -84.12),
                                    y_limits = c(46.45, 46.47),
                                    resolution = 100)

temp_dir <- tempdir()

# Test kmz out
phm_kmz_out <- suppressMessages(position_heat_map(lamprey_tracks, 
                                                  x_limits = c(-84.14, -84.12),
                                                  y_limits = c(46.45, 46.47),
                                                  resolution = 100,
                                                  output = "kmz",
                                                  folder = temp_dir))

phm_kmz_out_nameShouldBe <- normalizePath(file.path(temp_dir, 
                                                    "fish_absolute.kmz"),
                                          mustWork = FALSE)

phm_kmz_out_named <- suppressMessages(position_heat_map(lamprey_tracks, 
                                                        x_limits = c(-84.14, -84.12),
                                                        y_limits = c(46.45, 46.47),
                                                        resolution = 100,
                                                        output = "kmz",
                                                        folder = temp_dir,
                                                        out_file = "mymap"))

phm_kmz_out_named_nameShouldBe <- normalizePath(file.path(temp_dir, 
                                                          "mymap.kmz"),
                                                mustWork = FALSE)


# Test png out
phm_png_out <- suppressMessages(position_heat_map(lamprey_tracks, 
                                    x_limits = c(-84.14, -84.12),
                                    y_limits = c(46.45, 46.47),
                                    resolution = 100,
                                    output = "png",
                                    folder = temp_dir))

phm_png_out_nameShouldBe <- normalizePath(file.path(temp_dir, 
                                                     "fish_absolute.png"),
                                          mustWork = FALSE)

phm_png_out_named <- suppressMessages(position_heat_map(lamprey_tracks, 
                                                  x_limits = c(-84.14, -84.12),
                                                  y_limits = c(46.45, 46.47),
                                                  resolution = 100,
                                                  output = "png",
                                                  folder = temp_dir,
                                                  out_file = "mymap"))

phm_png_out_named_nameShouldBe <- normalizePath(file.path(temp_dir, 
                                                      "mymap.png"),
                                                mustWork = FALSE)



# Expected results
phm_shouldBe <- 
  structure(c(43L, 20L, 23L, 19L, 21L, 19L, 25L, 
  17L, 21L, 19L, 12L, 16L, 13L, 8L, 8L, 9L, 3L, 2L, 3L, 2L, 1L, 
  4L, 2L, 35L, 19L, 20L, 18L, 22L, 20L, 21L, 14L, 15L, 18L, 14L, 
  22L, 11L, 5L, 9L, 4L, 4L, 4L, 2L, 1L, 2L, 2L, 3L, 30L, 16L, 12L, 
  11L, 15L, 14L, 12L, 16L, 10L, 13L, 16L, 13L, 14L, 10L, 8L, 4L, 
  NA, 2L, 4L, 4L, 5L, 4L, 3L, 22L, 13L, 9L, 10L, 11L, 10L, 14L, 
  18L, 16L, 19L, 11L, 13L, 11L, 9L, 9L, 6L, 4L, 5L, 4L, 3L, 5L, 
  4L, 1L, 23L, 5L, 7L, 9L, 10L, 10L, 12L, 14L, 17L, 12L, 9L, 7L, 
  5L, 5L, 5L, 4L, 7L, 4L, 2L, 2L, 4L, 3L, 3L, 20L, 4L, 3L, 5L, 
  8L, 10L, 13L, 16L, 12L, 15L, 13L, 11L, 7L, 6L, 4L, 5L, 3L, 2L, 
  1L, 1L, 4L, 4L, NA, 19L, 2L, 7L, 7L, 12L, 14L, 11L, 11L, 11L, 
  9L, 9L, 8L, 7L, 5L, 8L, 6L, 5L, 3L, 3L, 4L, 3L, 3L, 5L, 15L, 
  3L, 8L, 9L, 10L, 11L, 11L, 11L, 13L, 13L, 15L, 12L, 9L, 5L, 5L, 
  9L, 3L, 5L, 6L, 4L, 4L, 4L, 4L, 18L, 7L, 7L, 7L, 7L, 12L, 6L, 
  12L, 10L, 11L, 15L, 17L, 11L, 9L, 11L, 3L, 7L, 7L, 5L, 5L, 7L, 
  1L, 2L, 17L, 4L, 7L, 9L, 8L, 6L, 9L, 10L, 9L, 12L, 11L, 7L, 7L, 
  8L, 10L, 10L, 8L, 5L, 1L, 5L, 5L, 6L, 2L, 16L, 7L, 2L, 5L, 8L, 
  8L, 6L, 7L, 9L, 8L, 10L, 6L, 7L, 9L, 7L, 8L, 4L, 6L, 8L, 8L, 
  7L, 3L, 3L, 17L, 6L, 4L, 8L, 7L, 8L, 6L, 9L, 10L, 9L, 13L, 8L, 
  10L, 10L, 12L, 9L, 11L, 9L, 13L, 8L, 2L, 3L, 4L, 17L, 2L, 5L, 
  9L, 10L, 6L, 9L, 9L, 12L, 8L, 9L, 7L, 11L, 5L, 6L, 11L, 8L, 10L, 
  7L, 2L, 4L, 5L, 4L, 18L, 3L, 7L, 5L, 5L, 7L, 8L, 3L, 7L, 9L, 
  8L, 12L, 12L, 15L, 12L, 12L, 8L, 7L, 5L, 5L, 6L, 2L, 1L, 12L, 
  8L, 7L, 5L, 9L, 5L, 10L, 9L, 4L, 6L, 12L, 14L, 9L, 14L, 15L, 
  7L, 5L, 4L, 5L, 5L, 3L, 6L, 4L, 12L, 7L, 7L, 7L, 10L, 5L, 9L, 
  11L, 14L, 15L, 16L, 11L, 14L, 15L, 6L, 4L, 6L, 5L, 5L, 2L, 3L, 
  6L, 6L, 26L, 12L, 15L, 20L, 28L, 31L, 36L, 37L, 46L, 49L, 54L, 
  57L, 60L, 63L, 58L, 49L, 47L, 48L, 39L, 33L, 25L, 21L, 19L), .Dim = c(23L, 
  17L), .Dimnames = structure(list(c("5150222", "5150122", "5150022", 
  "5149922", "5149822", "5149722", "5149622", "5149522", "5149422", 
  "5149322", "5149222", "5149122", "5149022", "5148922", "5148822", 
  "5148722", "5148622", "5148522", "5148422", "5148322", "5148222", 
  "5148122", "5148022"), c("719569", "719669", "719769", "719869", 
  "719969", "720069", "720169", "720269", "720369", "720469", "720569", 
  "720669", "720769", "720869", "720969", "721069", "721169")), .Names = c("", 
  "")))


# Testing output gives expected values
test_that("full vps data set gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(phm_full_input$values, phm_shouldBe)
})

test_that("data frame with min required columns gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(phm_reduced_input$values, phm_shouldBe)
})

test_that("data.table input gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(phm_dt_input$values, phm_shouldBe)
})

test_that("data.table input gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(phm_dt_input$values, phm_shouldBe)
})

test_that("png output default name gives expected result", {
  # Check if png file produced
  expect_true(file.exists(phm_png_out_nameShouldBe))
})

test_that("png output custom name gives expected result", {
  # Check if png file produced
  expect_true(file.exists(phm_png_out_named_nameShouldBe))
})

test_that("kmz output default name gives expected result", {
  # Check if png file produced
  expect_true(file.exists(phm_kmz_out_nameShouldBe))
})

test_that("kmz output custom name gives expected result", {
  # Check if png file produced
  expect_true(file.exists(phm_kmz_out_named_nameShouldBe))
})

