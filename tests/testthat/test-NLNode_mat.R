test_that("Append NLNode to non-living nodes in rows and columns of matrices", {
  # Create test data
  test_mat <- data.frame(
    X = c("Detritus", "Plant", "Invertebrate", "Vertebrate"),
    Detritus = c(NA, 1, 1, 1),
    Plant = c(NA, NA, NA, NA),
    Invertebrate = c(1, 1, NA, NA),
    Vertebrate = c(NA, NA, 1, NA)
  )
  rownames(test_mat) <- test_mat[, 1]
  test_mat <- test_mat[, -1]  # Remove first column

  # Test with single NLNode
  nlnode_single <- "Detritus"
  result1 <- NLNode_mat(test_mat, NLNode = nlnode_single)

  # Check that Detritus has NLNode appended
  expect_match(rownames(result1)[rownames(result1) == "DetritusNLNode"], "NLNode")

  # Check that non-NLNodes don't have NLNode appended
  non_nlnodes <- c("Plant", "Invertebrate", "Vertebrate")
  expect_equal(rownames(result1)[2:4], non_nlnodes)

  # Test with multiple NLNodes
  nlnodes_multiple <- c("Detritus", "Plant")
  result2 <- NLNode_mat(test_mat, NLNode = nlnodes_multiple)

  # Check that both Detritus and Plant have NLNode appended
  expect_true(all(c("DetritusNLNode", "PlantNLNode") %in% rownames(result2)))

  # Check that non-NLNodes don't have NLNode appended
  expect_equal(rownames(result2)[3:4], c("Invertebrate", "Vertebrate"))
})
