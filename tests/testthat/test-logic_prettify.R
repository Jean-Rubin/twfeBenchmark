test_that("pp_col relabel correctly some names", {
  expect_identical(pp_col("control"), "Never Treated")
  expect_identical(pp_col("treated"), "Treated")
  expect_identical(pp_col("treated_1"), "Treated 1")
  expect_identical(pp_col("treated_12_34"), "Treated 12 34")
  expect_identical(pp_col("12"), "12")
  expect_identical(pp_col("12_34"), "12 34")
  expect_identical(
    pp_col(c("control", "treated")),
    c("Never Treated", "Treated")
  )
  expect_identical(
    pp_col(c("control_1", "treated_2_12")),
    c("Never Treated 1", "Treated 2 12")
  )
})

test_that("unpp_col unrelabel correctly some names", {
  expect_identical(unpp_col("Never Treated"), "control")
  expect_identical(unpp_col("Treated"), "treated")
  expect_identical(unpp_col("Treated 1"), "treated_1")
  expect_identical(unpp_col("Treated 12 34"), "treated_12_34")
  expect_identical(unpp_col("12"), "12")
  expect_identical(unpp_col("12 34"), "12_34")
  expect_identical(
    unpp_col(c("Never Treated", "Treated")),
    c("control", "treated")
  )
  expect_identical(
    unpp_col(c("Never Treated 1", "Treated 2 12")),
    c("control_1", "treated_2_12")
  )
})

# property
test_that("pp_col reverse transformations of unpp_col", {
  expect_identical(pp_col(unpp_col("Never Treated")), "Never Treated")
  expect_identical(pp_col(unpp_col("Treated")), "Treated")
  expect_identical(pp_col(unpp_col("Treated 1")), "Treated 1")
  expect_identical(pp_col(unpp_col("Treated 12")), "Treated 12")
  expect_identical(pp_col(unpp_col("Treated 1 2")), "Treated 1 2")
  expect_identical(
    pp_col(unpp_col(c("Treated", "Treated 1 2"))),
    c("Treated", "Treated 1 2")
  )
})

# property
test_that("unpp_col reverse transformations of unpp_col", {
  expect_identical(unpp_col(pp_col("control")), "control")
  expect_identical(unpp_col(pp_col("treated")), "treated")
  expect_identical(unpp_col(pp_col("treated_1")), "treated_1")
  expect_identical(unpp_col(pp_col("treated_12")), "treated_12")
  expect_identical(unpp_col(pp_col("treated_1_2")), "treated_1_2")
  expect_identical(
    unpp_col(pp_col(c("treated", "treated_1_2"))),
    c("treated", "treated_1_2")
  )
})

