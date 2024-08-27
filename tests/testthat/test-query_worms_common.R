test_that("can be silenced", {
  expect_silent(
    query_worms_common("striped bass", silent = TRUE)
  )
})

test_that("outputs url to console if not silenced", {
  expect_output(
    query_worms_common("weakfish"),
    "https://www.marinespecies.org/rest/AphiaRecordsByVernacular/weakfish"
  )
})

test_that("encodes spaces in URL", {
  expect_output(
    query_worms_common("Atlantic sturgeon"),
    "%20"
  )
})

test_that("returns list", {
  expect_type(
    query_worms_common("Atlantic cod", silent = TRUE),
    "list"
  )
})
