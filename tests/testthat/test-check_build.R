test_that("check if the limfile is included or not", {
  expect_error(check_build(file = NULL), "No LIM Declaration File provided. Please check")
})
