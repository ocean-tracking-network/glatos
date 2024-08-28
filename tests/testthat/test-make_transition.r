
### make_transition
# Testing water polygon transition layer
test_that("make_transition: Transition layer for Higgins Lake water polygon as expected", {
    
  water <- suppressMessages(
    make_transition(
      higgins_lake_polygon,
      res = c(0.01, 0.01)
    )$transition
  )
  
  expect_s3_class(water, NA)

  expect_equal(dim(water), c(9, 11, 1))

  expect_s4_class(
    water,
    "TransitionLayer"
  )
  
  expect_s4_class(
    water@transitionMatrix,
    "dsCMatrix"
  )
  
  expect_length(water@transitionCells, 99)


  expect_snapshot(
    water
  )
})

# Testing water polygon raster
test_that("make_transition: Raster values for Higgins Lake water polygon as expected", {
 
   water <- suppressMessages(
     make_transition(
       higgins_lake_polygon,
       res = c(0.01, 0.01)
     )$rast
   ) 
   
  expect_s4_class(
    water,
    "RasterLayer"
  )
  expect_s3_class(water, NA)

  expect_equal(dim(water), c(9, 11, 1))

  expect_snapshot(
    water
  )
})



# Testing land polygon transition matrix
test_that("make_transition: Transition layer for Flynn Island land polygon as expected", {

  land <- suppressMessages(
    make_transition(
      flynn_island_polygon,
      res = c(0.001, 0.001)
    )$transition
  )

  expect_s4_class(
    land,
    "TransitionLayer"
  )
  expect_s3_class(land, NA)

  expect_equal(dim(land), c(6, 8, 1))

  expect_s4_class(
    land,
    "TransitionLayer"
  )
  
  expect_s4_class(
    land@transitionMatrix,
    "dsCMatrix"
  )
  expect_length(land@transitionMatrix, 2304)

  expect_snapshot(
    land
  )
})

# Testing land polygon raster
test_that("make_transition: Raster values for Flynn Island polygon as expected", {

  land <- suppressMessages(
    make_transition(
      flynn_island_polygon,
      res = c(0.001, 0.001)
    )$rast
  )

  expect_s4_class(
    land,
    "RasterLayer"
  )
  expect_s3_class(land, NA)

  expect_equal(dim(land), c(6, 8, 1))

  expect_snapshot(
    land
  )
})
