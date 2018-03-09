context("Testing impute_missing")

test_that("impute_missing(df, col, method, missing_val_char) returns a data frame or matrix without missing values
          with different methods", {

  # expected output
  expect_equal(
    impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, "?")),
                   "y",
                   "CC",
                   "?"),
    data.frame(x = c(1, 2), y = c(0, 10)))

  expect_equal(
    impute_missing(matrix(c(1,2,3, 0,10, NA), nrow = 3, ncol = 2, byrow = FALSE),
                   "V2",
                   "CC",
                   NA),
    data.frame("V1" = c(1, 2), "V2" = c(0, 10)))
  
  expect_equal(
    impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, NaN)),
                   "y",
                   "MIP",
                   NaN),
    data.frame(x = c(1, 2, 3), y = c(0, 10, 5)))

  expect_equal(
    impute_missing(matrix(c(1,2,3, 0,10,NA), nrow = 3, ncol = 2, byrow = FALSE),
                   "V2",
                   "MIP",
                   NA),
    data.frame(x = c(1, 2, 3), y = c(0, 10, 5)))
  
  expect_equal(
    impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, "?")),
                   "y",
                   "DIP",
                   "?"),
    data.frame(x = c(1, 2, 3), y = c(0, 10, 5)))
  
  expect_equal(
    impute_missing(matrix(c(1,2,3, 6,8,NA), nrow = 3, ncol = 2, byrow = FALSE),
                   "V2",
                   "DIP",
                   NA),
    data.frame(x = c(1, 2, 3), y = c(6, 8, 7)))

  # expected errors
  expect_error(impute_missing(list(1, 2, 2, NA), "V2", "MIP", NA), 
               "Error: data format is not supported, expected a data frame or a matrix")
  
  expect_error(impute_missing(data.frame("exp" = c(1, 2, 3), "res" = c(0, 10, NaN)),
                              res,
                              "CC",
                              NaN),
               "Error: column name is not applicable, expected a string instead")
  
  expect_error(impute_missing(data.frame("exp" = c(1, 2, 3), "res" = c(0, 10, "")),
                              "dn",
                              "CC",
                              ""),
               "Error: Error: the specified column name is not in the data frame")

  expect_error(impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, NA)), "y", "multi_im", NA),
               "method is not applicable")

  expect_error(impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, 0)), "y", "CC", 0),
               "Error: missing value format is not supported, expected one of blank space, "?", NA or NaN")
})
