test_that("Read all excel sheets, define as list, with sheet names as list element names", {

  net_data_input <- "autoLIMR_net_data_inputs_demo.xlsx"
  expect_type(read_all_sheets(filename = net_data_input, tibble = FALSE), "list")

  })
