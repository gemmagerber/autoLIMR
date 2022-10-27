test_that("Create list of demo adjacency matrix sheets for 4node", {
  demo <- demo_adj_mat()
  expect_type(demo, "list")
  for(i in length(demo)) {
    expect_s3_class(demo[[i]], "data.frame")
  }
})
