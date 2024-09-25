test_that("custom_palette works", {
  expect_equal(
    custom_palette()(8),
    c("#FF9300", "#1B9E77", "#7570B3", "#E7298A",
      "#66A61E", "#E6AB02", "#A6761D", "#666666")
  )

  expect_warning(
    custom_palette()(10)
  )
})
