test_that("drops array", {
  expect_identical(
    extract_station("HFX_HFX028"),
    "HFX028"
  )
})

test_that("handles vectors", {
  expect_identical(
    extract_station(c("HFX_HFX028", "WAKA_WAKA4")),
    c("HFX028", "WAKA4")
  )
})
