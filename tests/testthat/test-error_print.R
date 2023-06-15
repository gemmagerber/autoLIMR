test_that("errors print", {

  net_data_input <- 'test.xlsx'
  adj_mat_input <- 'test.xlsx'
  net_data_input_filetype <- sub(net_data_input,
                                 pattern = ".*\\.(.*)",
                                 replacement = "\\1")

  adj_mat_input_filetype <- sub(adj_mat_input,
                                pattern = ".*\\.(.*)",
                                replacement = "\\1")

  expect_error(error_print(net_data_input = NULL, adj_mat_input = NULL))
  expect_error(error_print(net_data_input = NULL))
  expect_error(error_print(adj_mat_input = NULL))
  expect_error(error_print(grepl(net_data_input, pattern = '\\.csv$|\\.xlsx$') == FALSE))
  expect_error(error_print(grepl(adj_mat_input, pattern = '\\.csv$|\\.xlsx$') == FALSE))
  expect_error(error_print(net_data_input_filetype != adj_mat_input_filetype))

  # expect_message(error_print(net_data_input_filetype == adj_mat_input_filetype))

  })

