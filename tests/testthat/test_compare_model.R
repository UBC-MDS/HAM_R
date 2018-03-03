context("Testing impute_missing")

test_that("impute_missing(df, method) returns a data frame without missing values
          with different methods", {

  # expected output
  expect_equal(
    impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, NA)),
                   "CC"),
    data.frame(x = c(1, 2), y = c(0, 10)))

  expect_true(
    impute_missing(matrix(c(1,2,3, 0,10,NA), nrow = 3, ncol = 2, byrow = FALSE),
                   "CC")
    ==
      matrix(c(1,2, 0,10), nrow = 2, ncol = 2, byrow = FALSE))

  expect_true(
    impute_missing(c(1, NA, 2, 3), "CC") == c(1, 2, 3))

  expect_equal(
    impute_missing(data.frame(x = c(1, 2, 3, 4), y = c(0, 6, 10, NA)),
                   "mean_im"),
    data.frame(x = c(1, 2, 3, 4), y = c(0, 6, 10, 4)))

  expect_true(
    impute_missing(matrix(c(1,2,3,4, 0,6,10,NA), nrow = 4, ncol = 2, byrow = FALSE),
                   "mean_im")
    ==
      matrix(c(1,2,3,4, 0,6,10,4), nrow = 4, ncol = 2, byrow = FALSE))

  expect_true(
    impute_missing(c(1, NA, 2, 5), "mean_im") == c(1, 2, 2, 5))

  expect_equal(
    impute_missing(data.frame(x = c(1, 2, 3, 4), y = c(0, 10, 10, NA)),
                   "most_freq"),
    data.frame(x = c(1, 2, 3, 4), y = c(0, 10, 10, 10)))

  expect_true(
    impute_missing(matrix(c(1,2,3,4, 0,10,10,NA), nrow = 4, ncol = 2, byrow = FALSE),
                   "most_freq")
    ==
      matrix(c(1,2,3,4, 0,10,10,10), nrow = 4, ncol = 2, byrow = FALSE))

  expect_true(
    impute_missing(c(1, 2, NA, 2, 5), "most_freq") == c(1, 2, 2, 2, 5))

  # expected errors
  expect_true(
    impute_missing(list(1, 2, 2, NA), "most_freq") == list(1, 2, 2, 2))

  expect_equal(
    impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, NA)),
                   "CC"),
    data.frame(x = c(1, 2, 3)))

  expect_true(
    impute_missing(c(1, 2, 3, NA), "CC") == impute_missing(c(1, 2, 3, ""), "CC"))
})
