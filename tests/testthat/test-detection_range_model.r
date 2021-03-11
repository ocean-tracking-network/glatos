context("Check detection_range_model")

# test to see if each model runs properly -----

test_that("Test if polynomial model for model_frame = 'data_frame', is correct", {
  
  m <- detection_range_model(avg_percent ~ -1 + distance_m + I(distance_m ^ 2) + 
                               I(distance_m ^ 3) + offset(intercept),
                             data = sample_detection_efficiency, 
                             p = c(50),
                             link = "polynomial", 
                             model_frame = "data_frame", 
                             summary_stats = TRUE)
  
  expect_equal(m$distance, expected = 371)
  expect_equal(m$chi_square, expected = 1304, tolerance = 0.01)
  expect_equal(m$pgof, expected =  4.20e-281, tolerance = 0.01)
  expect_equal(m$a, expected = 0.20, tolerance = 0.01)
})

test_that("Test if polynomial model for model_frame = 'matrix', is correct", {
  m1 <- detection_range_model(avg_percent ~ -1 + poly(distance_m, 3, raw = TRUE) + 
                                offset(intercept), 
                              data = sample_detection_efficiency, 
                              p = c(50), 
                              link = "polynomial",
                              model_frame = "matrix",
                              summary_stats = TRUE)
  
  
  expect_equal(m1$distance, expected = 371)
  expect_equal(m1$chi_square, expected = 1304, tolerance = 0.01)
  expect_equal(m1$pgof, expected =  4.20e-281, tolerance = 0.01)
  expect_equal(m1$a, expected = 0.20, tolerance = 0.01)
  
  
  
})

test_that("Test if logit model is correct", {
  m2 <- detection_range_model(avg_percent_d ~ distance_m,
                              data = sample_detection_efficiency, 
                              p = c(50),
                              link = "logit",
                              summary_stats = TRUE)
  
  expect_equal(m2$distance, expected = 369, tolerance = 0.01)
  expect_equal(m2$chi_square, expected = 0.751, tolerance = 0.01)
  expect_equal(m2$pgof, expected =  0.980, tolerance = 0.01)
  expect_equal(m2$null_deviance, expected = 6.306, tolerance = 0.01)
  
  
})


test_that("Test if probit model is correct", {
  m3 <- detection_range_model(avg_percent_d ~ distance_m,
                              data = sample_detection_efficiency, 
                              p = c(50),
                              link = "probit",
                              summary_stats = TRUE)
  
  expect_equal(m3$distance, expected = 364, tolerance = 0.01)
  expect_equal(m3$pgof, expected =  0.973, tolerance = 0.01)
  expect_equal(m3$chi_square, expected = 0.864, tolerance = 0.01)
  expect_equal(m3$null_deviance, expected = 6.31, tolerance = 0.01)
  
  
})
