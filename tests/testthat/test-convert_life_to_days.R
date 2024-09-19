test_that("converts number stored as text", {
  life_text <- convert_life_to_days("5")

  expect_equal(life_text, 5)
  expect_type(life_text, "integer")
})

test_that("converts number stored as numeric", {
  life_numeric <- convert_life_to_days(5)

  expect_equal(life_numeric, 5)
  expect_type(life_numeric, "integer")
})

test_that("strips 'days'", {
  life_days <- convert_life_to_days("10 days")

  expect_equal(life_days, 10)
  expect_type(life_days, "integer")
})

test_that("errors if different unit", {
  expect_error(
    convert_life_to_days("10 weeks"),
    "Cannot convert 10 weeks to time days."
  )

  expect_error(
    convert_life_to_days("parsecs"),
    "Cannot convert parsecs to time days."
  )
})

test_that("handles vectors", {
  expect_identical(
    convert_life_to_days(c("5", 5, "10 days")),
    as.integer(c(5, 5, 10))
  )
})
