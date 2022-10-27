test_that("Create demo network input data sheets for 4node", {
  demo <- demo_net_input()
  expect_type(demo, "list")
  for(i in length(demo)) {
    expect_s3_class(demo[[i]], "data.frame")
  }
})
