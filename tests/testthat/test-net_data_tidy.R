test_that("Tidies up network data input sheet", {

  example_net <- data.frame(
    Compartment = c("Detritus", "Plant", "Invertebrate", "Vertebrate"),
    Biomass = c(10000.2, 800, 2000, 55),
    Consumption_lower = c(NA, 0.05, 100, NA),
    Consumption_upper = c(NA, 0.6, 10000, NA),
    Production_lower = c(NA, 0.6, "0.004 * Invertebrate_U", 0.0003),
    Production_upper = c(NA, 1.1, "0.06 * Invertebrate_U", 0.005),
    Respiration_lower = c(NA, "0.4 * Plant_NPP", "1 * Invertebrate_P", 0.75),
    Respiration_upper = c(NA, "0.7 * Plant_NPP", "5 * Invertbrate_P", 0.75),
    Unused_energy_lower = c(NA, NA, NA, 0.5),
    Unused_energy_upper = c(NA, NA, NA, 0.6),
    Imports_lower = c(1, 1, NA, NA),
    Imports_upper = c(NA, NA, NA, NA),
    Exports_lower = c(1, 1, NA, 1),
    Exports_upper = c(NA, NA, NA, NA),
    AE_lower = c(NA, NA, 0.5, 0.2),
    AE_upper = c(NA, NA, 0.8, 0.3)
  )

  tidy_net <- net_data_tidy(x = example_net, NLNode = "Detritus")

expect_no_match(tidy_net, " ", all = FALSE)
expect_no_match(tidy_net, ",", all = FALSE)

  })
