test_that("sci_notation_off() turns off scientific notation", {
  x <- data.frame(
    X = c("Detritus", "Plant", "Invertebrate", "Vertebrate"),
    Detritus = c(NA, 1, 1, 1),
    Plant = c(NA, NA, NA, NA),
    Invertebrate = c(1, 1, NA, NA),
    Vertebrate = c(NA, NA, 1, NA)
  )
  rownames(x) <- x[, 1] # Make Compartment Name the Row Name
  x <- x[, 2:4]
  x <- as.character(unlist(as.vector(x)))

  expect_no_match(x, "E", all = FALSE)
})
