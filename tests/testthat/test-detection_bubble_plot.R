# R/vis-detection_bubble_plot.r


test_that("detection_bubble_plot works", {
  
  
  det <- walleye_detections
  
  temp_png <- file.path(tempdir(), "dbp.png")
  
  vdiffr::expect_doppelganger("dbp-basic-functionality",
      fig = function()
              detection_bubble_plot(det)
  )
  
  expect_message(
    dbp <- detection_bubble_plot(det,
                                 out_file = temp_png),
   "Image files were written to the following directory\\:")
    
  # check image written to disk
  expect_snapshot_file(temp_png)
  
  # check returned value
  expect_equal(dbp, dbp_shouldbe)
  
  
  # Clean up
  unlink(
    temp_png
  )
  
})
