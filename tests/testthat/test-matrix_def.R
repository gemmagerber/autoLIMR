test_that("Return import/export matrix based on search_cols function", {
  input <- data.frame(
    Compartment = c("Det", "Plant", "Invert", "Vert"),
    Biomass = c(10000.2, 800, 2000, 55),
    Consumption_lower = c(NA, 1100, 100, NA),
    Consumption_upper = c(NA, 2600, 3000, NA),
    Production_lower = c(NA, "0.6 * Plant_GPP", "0.4 * Invert_Q", NA),
    Production_upper = c(NA, "0.8 * Plant_GPP", "0.6 * Invert_Q", NA),
    Respiration_lower = c(NA, "0.4 * Plant_NPP", "1 * Invert_P", 0.75),
    Respiration_upper = c(NA, "0.7 * Plant_NPP", NA, 0.75),
    Unused_energy_lower = c(NA, NA, NA, NA),
    Unused_energy_upper = c(NA, NA, NA, "0.05 * Vert_Q"),
    Imports_lower = c(1, 1, NA, NA),
    Imports_upper = c(NA, NA, NA, NA),
    Exports_lower = c(1, 1, NA, 1),
    Exports_upper = c(NA, NA, NA, NA),
    AE_lower = c(NA, NA, "0.5 * Invert_Q", "0.2 * Vert_Q"),
    AE_upper = c(NA, NA, NA, NA)
  )

  tidy <- net_data_tidy(x = input, NLNode = "Det")

  # Test that import matrix only contains columns with "Imports" in columns
  input_mat <- matrix_def(x = tidy, mat.type = "Import")
  in_pattern <-
    paste0(
      c(
        "Import",
        "Input",
        "^+In+$",
        "^+IN+$",
        "IN_",
        "IM_",
        "^+IM+$",
        "^+Im+$"
      ),
      collapse = "|"
    )
  x <- all(grepl(in_pattern, colnames(input_mat)))
  expect_true(x)

  # Test that export matrix only contains columns with "Exports" in columns
  export_mat <- matrix_def(x = tidy, mat.type = "Export")
  ex_pattern <-
    paste0(c("Export", "Exports", "Ex", "EX", "EX_"), collapse = "|")
  y <- all(grepl(ex_pattern, colnames(export_mat)))
  expect_true(y)
})
