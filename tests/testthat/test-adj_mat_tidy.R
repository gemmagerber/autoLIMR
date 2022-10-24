test_that("Tidies up adjacency matrix input sheet", {

  x <- data.frame(
    X = c("Detritus", "Plant", "Invertebrate", "Vertebrate"),
    Detritus = c(NA, 1, 1, 1),
    Plant = c(NA, NA, NA, NA),
    Invertebrate = c(1, 1, NA, NA),
    Vertebrate = c(NA, NA, 1, NA)
  )
  rownames(x) <- x[, 1] # Make Compartment Name the Row Name

  new_mat <- NLNode_mat(x = x, NLNode = "Detritus")


  tidy_net <- adj_mat_tidy(x = x, NLNode = "Detritus")

  expect_no_match(tidy_net, " ", all = FALSE)
  expect_no_match(tidy_net, ",", all = FALSE)

})
