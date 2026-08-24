test_that("OSEM transformations round trip", {
  values <- c(-2, 0, 3)
  expect_equal(
    osem:::inverse_transform_osem_values(
      osem:::transform_osem_values(values, "asinh"),
      "asinh"
    ),
    values
  )

  positive <- c(0.5, 2, 8)
  expect_equal(
    osem:::inverse_transform_osem_values(
      osem:::transform_osem_values(positive, "log"),
      "log"
    ),
    positive
  )
})
