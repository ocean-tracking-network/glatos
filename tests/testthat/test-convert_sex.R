test_that("converts M/MALE/Male/male and F/FEMALE/Female/female", {
  sex <- convert_sex(c(
    "M",
    "MALE",
    "Male",
    "male",
    "F",
    "FEMALE",
    "Female",
    "female"
  ))

  expect_identical(sex, c(rep("MALE", 4), rep("FEMALE", 4)))
})

test_that("can handle a vector", {
  sex_vec <- convert_sex(c("FEMALE", "MALE"))

  expect_identical(sex_vec, c("FEMALE", "MALE"))
  expect_type(sex_vec, "character")
})

test_that("handles missing values correctly", {
  sex_na <- convert_sex(NA)

  expect_identical(sex_na, NA)
  expect_type(sex_na, "logical")
})
