test_that("demo data is created", {
  # If demo files are needed
  expect_message(
    demo_data(net_data_input = "demo", adj_mat_input = "demo"),
    message(
      strwrap(
        prefix = " \n",
        initial = "",
        "The 4node demo datasets (1. network input data and 2. adjacency
        matrix input) have been created and saved in the
        working directory."
      )
    )
  )
})
