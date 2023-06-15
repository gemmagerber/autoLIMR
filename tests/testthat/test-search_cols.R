test_that("search_col function returns a vector of given length for each
          Export, Import, AE",
          {
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

            # Test Exports
            testex <- search_cols(x = tidy, col.match = "Export")
            expect_vector(object = testex, size = 2)
            for (i in length(testex)) {
              expect_true(grepl(x = testex[i], pattern = paste0(
                c("Export", "Exports", "Ex", "EX", "EX_"),
                collapse = "|"
              )))
            }

            # Test Imports
            testin <- search_cols(x = tidy, col.match = "Import")
            expect_vector(object = testin, size = 2)
            for (i in length(testin)) {
              expect_true(grepl(x = testin[i],
                                pattern = paste0(
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
                                )))
            }

            # Test AE
            testae <- search_cols(x = tidy, col.match = "AE")
            expect_vector(object = testae, size = 2)
            for (i in length(testae)) {
              expect_true(grepl(x = testae[i],
                                pattern = paste0(
                                  c("AE", "Assimilation", "efficiency", "AssEm"),
                                  collapse = "|"
                                )))
            }


          })
