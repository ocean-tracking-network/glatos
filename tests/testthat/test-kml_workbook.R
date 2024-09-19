# R/vis-kml_workbook.r

test_that("kml_workbook works", {
  temp_kml <- file.path(tempdir(), "wb.kml")

  # read example object
  # kml output
  expect_snapshot(
    kml1 <- kml_workbook(
      wb = walleye_workbook,
      out_file = temp_kml
    )
  )


  # kmz output
  # read workbook directly; output kmz
  temp_kmz <- file.path(tempdir(), "wb.kmz")

  expect_snapshot(
    kml2 <- kml_workbook(
      wb = walleye_workbook,
      kmz = TRUE,
      out_file = temp_kmz
    )
  )


  # bigger label and point size
  temp_kml_bigger <- file.path(tempdir(), "wb_bigger.kml")

  expect_snapshot(
    kml3 <- kml_workbook(
      wb = walleye_workbook,
      labelSize = 20,
      iconSize = 1,
      out_file = temp_kml_bigger
    )
  )


  # read from file
  wb_file <- system.file("extdata",
    "walleye_workbook.xlsm",
    package = "glatos"
  )


  # copy to temp location
  temp_wb_file <- file.path(tempdir(), basename(wb_file))
  file.copy(wb_file, temp_wb_file)


  # write to same location (default)
  expect_snapshot(
    kml4 <- kml_workbook(
      wb_file = temp_wb_file
    )
  )

  # clean up
  unlink(
    c(
      temp_kml,
      temp_kmz,
      temp_kml_bigger,
      temp_wb_file,
      kml4
    )
  )
})
