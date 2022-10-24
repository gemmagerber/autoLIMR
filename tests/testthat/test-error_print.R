test_that("errors print", {

  expect_error(error_print(net_data_input = NULL, adj_mat_input = NULL))
  expect_error(error_print(net_data_input = NULL))
  expect_error(error_print(adj_mat_input = NULL))

})
