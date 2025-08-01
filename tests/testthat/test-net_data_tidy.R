test_that("net_data_tidy handles data correctly", {
  # Setup test data with intentional spaces, commas, zeros, and NAs
  example_net <- data.frame(
    Compartment = c("Detritus", "Plant", "Invertebrate", "Vertebrate", "Zero Biomass"),
    Biomass = c(10000.2, 800, 2000, 55, 0),
    Consumption_lower = c(NA, 0.05, 100, NA, 0.1),
    Consumption_upper = c(NA, "0,6", 10000, NA, 0.2),
    Production_lower = c(NA, 0.6, "0.004 * Invertebrate_U", 0.0003, 0.1),
    Production_upper = c(NA, 1.1, "0.06 * Invertebrate_U", 0.005, 0.2),
    Respiration_lower = c(NA, "0.4 * Plant_NPP", "1 * Invertebrate_P", 0.75, 0.1),
    Respiration_upper = c(NA, "0.7 * Plant_NPP", "5 * Invertbrate_P", 0.75, 0.2),
    Unused_energy_lower = c(NA, NA, NA, 0.5, 0.1),
    Unused_energy_upper = c(NA, NA, NA, 0.6, 0.2),
    stringsAsFactors = FALSE
  )

  # Call the function with Detritus as NLNode
  tidy_net <- net_data_tidy(x = example_net, NLNode = "Detritus")

  # 1. Test that the output is a data frame
  expect_true(is.data.frame(tidy_net))

  # 2. Test that NLNode was properly appended
  expect_true("DetritusNLNode" %in% rownames(tidy_net))
  expect_false("Detritus" %in% rownames(tidy_net))

  # 3. Test that rows with zero biomass were removed
  expect_false("ZeroBiomass" %in% rownames(tidy_net))
  expect_equal(nrow(tidy_net), 4) # Should be 4 rows (not 5)

  # 4. Test that commas were replaced with periods
  # The "0,6" value should become "0.6"
  expect_equal(tidy_net["Plant", "Consumption_upper"], "0.6")

  # 5. Test that spaces were removed
  # Convert to character vector to check for spaces
  all_values <- unlist(lapply(tidy_net, as.character))
  expect_false(any(grepl(" ", all_values)))

  # 6. Test handling of empty input
  expect_equal(nrow(net_data_tidy(NULL, NLNode = "Detritus")), 0)
  expect_equal(nrow(net_data_tidy(data.frame(), NLNode = "Detritus")), 0)

  # 7. Test with multiple NLNodes
  tidy_net2 <- net_data_tidy(x = example_net, NLNode = c("Detritus", "Plant"))
  expect_true("DetritusNLNode" %in% rownames(tidy_net2))
  expect_true("PlantNLNode" %in% rownames(tidy_net2))

  # 8. Test with no NLNodes
  tidy_net3 <- net_data_tidy(x = example_net, NLNode = NULL)
  expect_true("Detritus" %in% rownames(tidy_net3))
  expect_false(any(grepl("NLNode", rownames(tidy_net3))))

  # 9. Test column preservation (except first)
  expect_setequal(
    colnames(tidy_net),
    setdiff(colnames(example_net), "Compartment")
  )
})
