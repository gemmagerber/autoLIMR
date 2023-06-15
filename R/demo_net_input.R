#' @title demo_net_input()
#'
#' @description Create a named list of demo network input data sheets for
#' four-node seasonal network
#'
#' @return A list containing 4 seasons worth of network input data
#' @export
#' @examples
#' x <- demo_net_input()
#'
demo_net_input <- function() {
  net_summer <- data.frame(
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

  net_autumn <- data.frame(
    Compartment = c("Det", "Plant", "Invert", "Vert"),
    Biomass = c(8000, 600, 900.1, 33),
    Consumption_lower = c(NA, 850, NA, NA),
    Consumption_upper = c(NA, 1700, NA, NA),
    Production_lower = c(NA, "0.4 * Plant_GPP", "0.2 * Invert_Q", 0.0002),
    Production_upper = c(NA, "0.9 * Plant_GPP", "0.4 * Invert_Q", NA),
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
    Biomass = c(7111, 500, 822, 222),
    Consumption_lower = c(NA, 700, NA, NA),
    Consumption_upper = c(NA, 1000, NA, NA),
    Production_lower = c(NA, NA, NA, NA),
    Production_upper = c(NA, NA, NA, NA),
    Respiration_lower = c(NA, NA, NA, NA),
    Respiration_upper = c(NA, NA, NA, NA),
    Unused_energy_lower = c(NA, NA, NA, NA),
    Unused_energy_upper = c(NA, NA, NA, NA),
    Imports_lower = c(200, 700, 1, 1),
    Imports_upper = c(1200, 1300, NA, NA),
    Exports_lower = c(180, 300, NA, 1),
    Exports_upper = c(380, 1300, NA, 1)
  )

  net_spring <- data.frame(
    Compartment = c("Det", "Plant", "Invert", "Vert"),
    Biomass = c(9000, 700.5, 1000, 44),
    Consumption_lower = c(NA, 950, NA, NA),
    Consumption_upper = c(NA, 1850, NA, NA),
    Production_lower = c(NA, "0.5 * Plant_GPP", "0.3 * Invert_Q", 0.0003),
    Production_upper = c(NA, "1 * Plant_GPP", "0.5 * Invert_Q", NA),
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
