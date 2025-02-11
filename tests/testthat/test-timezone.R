test_that("timezone is correct", {
  expect_equal(Sys.timezone(), "UTC")
})
