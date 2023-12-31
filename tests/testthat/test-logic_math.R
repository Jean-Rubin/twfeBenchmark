test_that("min_max works on single values", {
  expect_equal(min_max(0.2, 0, 1), 0.2)
  expect_identical(min_max(-0.1, 0, 1), 0)
  expect_identical(min_max(1.1, 0, 1), 1)
})

test_that("min_max works on vector values", {
  expect_equal(min_max(c(0.2, -0.1, 1.1), 0, 1), c(0.2, 0, 1))
})
