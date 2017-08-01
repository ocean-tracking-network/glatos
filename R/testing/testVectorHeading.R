# To run:
# test_file("/Users/dinian/Desktop/glatos-git/R/testing/testVectorHeading.R")

# Sample data for vectorHeading.R
x1 <- c(2, 4)
y1 <- c(2, 4)
x2 <- c(2, 4, 2)
y2 <- c(2, 4, 2)
vec1 <- vectorHeading(x1, y1)
vec2 <- vectorHeading(x2, y2)

# Expected results
vec1ShouldBe <- 45
vec2ShouldBe <- c(45, -135)

# Testing vectorHeading using testthat library
test_that("First vector gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(vec1, vec1ShouldBe)
})
test_that("Second vector gives expected result", {
  # Check if expected and actual results are the same
  expect_equal(vec2, vec2ShouldBe)
})