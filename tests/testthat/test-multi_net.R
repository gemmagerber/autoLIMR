test_that("function returns an object of class 'multi_net_ouput'", {
  fpath <- system.file("example_limfiles",
  "Winter_Weighted_Network_LIMfile.R",
  package = "autoLIMR")

  x <- multi_net(file = fpath, iter = 10, jmp = 1, x0 = NULL, pack = FALSE)

  expect_success(expect_type(x, "list"))
  expect_s3_class(x, "multi_net_output")


})
