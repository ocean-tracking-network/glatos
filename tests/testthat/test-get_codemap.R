test_that("returns codemap", {
  expect_equal(
    get_codemap("A69-1601-12345"),
    "A69-1601"
  )
})

test_that("handles vectors", {
  expect_identical(
    get_codemap(c("A69-1601-12345", "A69-1601-54321", "A69-1303-2222")),
    c("A69-1601", "A69-1601", "A69-1303")
  )
})
