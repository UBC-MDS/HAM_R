context("Testing impute_missing")

test_that("impute_missing(df, method, missing_val_char) returns a data frame or matrix without missing values
          with different methods", {

  # expected output
  expect_equal(
    impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, NA)),
                   "CC",
                   NA),
    data.frame(x = c(1, 2), y = c(0, 10)))

  expect_true(
    impute_missing(matrix(c(1,2,3, 0,10, NA), nrow = 3, ncol = 2, byrow = FALSE),
                   "CC",
                   NA)
    ==
      matrix(c(1,2, 0,10), nrow = 2, ncol = 2, byrow = FALSE))

  expect_true(
    impute_missing(matrix(c(1,2,3,4, 0,6,10,NA), nrow = 4, ncol = 2, byrow = FALSE),
                   "mean_im",
                   NA)
    ==
      matrix(c(1,2,3,4, 0,6,10,4), nrow = 4, ncol = 2, byrow = FALSE))
  
  expect_true(
    impute_missing(matrix(c(1,2,3,4, 0,10,10,NA), nrow = 4, ncol = 2, byrow = FALSE),
                   "most_freq",
                   NA)
    ==
      matrix(c(1,2,3,4, 0,10,10,5), nrow = 4, ncol = 2, byrow = FALSE))

  expect_equal(
    impute_missing(data.frame(x = c(1, 2, 3, 4), y = c(0, 10, 10, NA)),
                   "most_freq",
                   NA),
    data.frame(x = c(1, 2, 3, 4), y = c(0, 10, 10, 10)))

  expect_true(
    impute_missing(matrix(c(1,2,3,4, 0,10,10,NA), nrow = 4, ncol = 2, byrow = FALSE),
                   "most_freq",
                   NA)
    ==
      matrix(c(1,2,3,4, 0,10,10,10), nrow = 4, ncol = 2, byrow = FALSE))

  # expected errors
  expect_error(impute_missing(list(1, 2, 2, NA), "most_freq", NA), 
               "Input type is not supported.")

  expect_error(impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, NA)), "multiple_im", NA),
               "Method name is unavailable.")

  expect_error(impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, 0)), "CC", 0),
               "Missing value type is not supported.")
})
