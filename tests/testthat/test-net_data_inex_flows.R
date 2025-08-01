test_that("net_data_inex_flows handles different combinations of imports and exports", {
  # Mock matrix_def function for testing
  local_mocked_bindings(
    matrix_def = function(x, mat_type) {
      if (mat_type == "Import" && x$has_imports) {
        result <- matrix(1, nrow = 2, ncol = 2)
        rownames(result) <- c("Commodity1", "Commodity2")
        return(result)
      } else if (mat_type == "Export" && x$has_exports) {
        result <- matrix(1, nrow = 3, ncol = 3)
        rownames(result) <- c("Commodity1", "Commodity2", "Commodity3")
        return(result)
      } else {
        return(matrix(nrow = 0, ncol = 0))
      }
    }
  )

  # Scenario 1: Both imports and exports exist
  test_data1 <- list(has_imports = TRUE, has_exports = TRUE)
  result1 <- net_data_inex_flows(test_data1)
  expected1 <- c(
    "! Import flows",
    "",
    "Commodity1_IM: Commodity1Import -> Commodity1",
    "Commodity2_IM: Commodity2Import -> Commodity2",
    "",
    "! Export flows",
    "",
    "Commodity1_EX: Commodity1 -> Commodity1Export",
    "Commodity2_EX: Commodity2 -> Commodity2Export",
    "Commodity3_EX: Commodity3 -> Commodity3Export",
    ""
  )
  expect_equal(result1, expected1)

  # Scenario 2: Imports exist, exports don't
  test_data2 <- list(has_imports = TRUE, has_exports = FALSE)
  result2 <- net_data_inex_flows(test_data2)
  expected2 <- c(
    "! Import flows",
    "",
    "Commodity1_IM: Commodity1Import -> Commodity1",
    "Commodity2_IM: Commodity2Import -> Commodity2",
    "! No export flows"
  )
  expect_equal(result2, expected2)

  # Scenario 3: Exports exist, imports don't
  test_data3 <- list(has_imports = FALSE, has_exports = TRUE)
  result3 <- net_data_inex_flows(test_data3)
  expected3 <- c(
    "! No import flows",
    "",
    "! Export flows",
    "",
    "Commodity1_EX: Commodity1 -> Commodity1Export",
    "Commodity2_EX: Commodity2 -> Commodity2Export",
    "Commodity3_EX: Commodity3 -> Commodity3Export",
    ""
  )
  expect_equal(result3, expected3)

  # Scenario 4: Neither imports nor exports exist
  test_data4 <- list(has_imports = FALSE, has_exports = FALSE)
  result4 <- net_data_inex_flows(test_data4)
  expected4 <- c("! No import flows", "! No export flows")
  expect_equal(result4, expected4)
})
