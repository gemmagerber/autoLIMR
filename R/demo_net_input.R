#' @title demo_net_input(): demo network data
#'
#' @description Create a named list of demo network input data sheets for
#' four-node seasonal network
#'
#' @return A list containing 4 seasons worth of network input data
#' @export
demo_net_input <- function() {
  net_summer <- data.frame(
    Compartment = c("Det", "Plant", "Invert", "V"),
    Biomass = c(10000.2, 800, 2000, 55),
    Consumption_lower = c(NA, 0.05, 100, NA),
    Consumption_upper = c(NA, 0.6, 10000, NA),
    Production_lower = c(NA, 0.6, "0.004 * Invert_U", 0.0003),
    Production_upper = c(NA, 1.1, "0.06 * Invert_U", 0.005),
    Respiration_lower = c(NA, "0.4 * Plant_NPP", "1 * Invert_P", 0.75),
    Respiration_upper = c(NA, "0.7 * Plant_NPP", "5 * Invert_P", 0.75),
    Unused_energy_lower = c(NA, NA, NA, 0.5),
    Unused_energy_upper = c(NA, NA, NA, 0.6),
    Imports_lower = c(1, 1, NA, NA),
    Imports_upper = c(NA, NA, NA, NA),
    Exports_lower = c(1, 1, NA, 1),
    Exports_upper = c(NA, NA, NA, NA),
    AE_lower = c(NA, NA, 0.5, 0.2),
    AE_upper = c(NA, NA, 0.8, 0.3)
  )

  net_autumn <- data.frame(
    Compartment = c("Det", "Plant", "Invert", "Vert"),
    Biomass = c(8000, 600, 900.1, 33),
    Consumption_lower = c(NA, 0.03, NA, NA),
    Consumption_upper = c(NA, 0.4, NA, NA),
    Production_lower = c(NA, 0.4, "0.002 * Invert_U", 0.0002),
    Production_upper = c(NA, 0.9, "0.04 * Invert_U", 0.003),
    Respiration_lower = c(NA, "0.2 * Plant_NPP", "0.8 * Invert_P", 0.35),
    Respiration_upper = c(NA, "0.5 * Plant_NPP", "3 * Invert_P", 0.35),
    Unused_energy_lower = c(NA, NA, NA, NA),
    Unused_energy_upper = c(NA, NA, NA, NA),
    Imports_lower = c(1, 1, 1, 1),
    Imports_upper = c(NA, NA, NA, NA),
    Exports_lower = c(1, 1, 1, 1),
    Exports_upper = c(NA, NA, NA, NA)
  )

  net_winter <- data.frame(
    Compartment = c("Det", "Plant", "Invert", "Vert"),
    Biomass = c(7000, 500, 800, 200),
    Consumption_lower = c(NA, 200, 500, NA),
    Consumption_upper = c(NA, 1000, 900, 350),
    Production_lower = c(NA, NA, NA, NA),
    Production_upper = c(NA, NA, NA, NA),
    Respiration_lower = c(NA, NA, 0.75, NA),
    Respiration_upper = c(NA, NA, 0.75, NA),
    Unused_energy_lower = c(NA, NA, NA, NA),
    Unused_energy_upper = c(NA, 200, NA, NA),
    Imports_lower = c(NA, 700, 1, NA),
    Imports_upper = c(1200, 1300, 1, NA),
    Exports_lower = c(NA, NA, NA, 1),
    Exports_upper = c(380, 1300, NA, 1),
    AE_lower = c(NA, NA, 0.5, 0.2),
    AE_upper = c(NA, NA, 0.8, 0.3)
  )

  net_spring <- data.frame(
    Compartment = c("Det", "Plant", "Invert", "Vert"),
    Biomass = c(9000, 700.5, 1000, 44),
    Consumption_lower = c(NA, 0.04, NA, NA),
    Consumption_upper = c(NA, 0.5, NA, NA),
    Production_lower = c(NA, 0.5, "0.003 * Invert_U", 0.0003),
    Production_upper = c(NA, 1, "0.005 * Invert_U", 0.004),
    Respiration_lower = c(NA, "0.3 * Plant_NPP", "0.9 * Invert_P", 0.65),
    Respiration_upper = c(NA, "0.6 * Plant_NPP", "4 * Invert_P", 0.65),
    Unused_energy_lower = c(NA, NA, NA, NA),
    Unused_energy_upper = c(NA, NA, NA, NA),
    Imports_lower = c(1, 1, 1, 1),
    Imports_upper = c(NA, NA, NA, NA),
    Exports_lower = c(1, 1, 1, 1),
    Exports_upper = c(NA, NA, NA, NA)
  )

  net_data <- list(
    "Summer" = net_summer,
    "Autumn" = net_autumn,
    "Winter" = net_winter,
    "Spring" = net_spring
  )

  return(net_data)
}
