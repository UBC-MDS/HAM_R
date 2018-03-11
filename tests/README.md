## Full Branch Coverage

`impute_missing()`

| Condition | Test Function |
|---|---|
| "data format is not supported, expected a data frame or a matrix" | `expect_error(impute_missing(list(1, 2, 2, NA), "V2", "MIP", NA))` |
| "column name is not applicable, expected a string instead" | `expect_error(impute_missing(data.frame("exp" = c(1, 2, 3), "res" = c(0, 10, NaN)))` |
| "the specified column name is not in the data frame" | `expect_error(impute_missing(data.frame(exp = c(1, 2, 3), res = c(0, 10, "")), "dn", "CC", ""))` |
| "method is not applicable" | `expect_error(impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, NA)), "y", "multi_im", NA))` |
| "output type is a dataframe" | `assert isinstance(impute_missing(np.matrix([[1, 2], [3, np.nan], [5, 6]]), 'b', "CC", np.nan), pd.DataFrame)` |
| "missing value format is not supported, expected one of blank space, a question mark, NA and NaN" | `expect_error(impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, 0)), "y", "CC", 0)` |
| "the input is a data frame, method is CC and missing_val_char is NA" | `expect_equal(impute_missing(data.frame(V1 = c(1, 2, 3), V2 = c(0, 10, NA)), "V2", "CC", NA), data.frame(V1 = c(1, 2), V2 = c(0, 10)))` |
| "the input is a matrix, method is CC and missing_val_char is NA" | `expect_equal(impute_missing(matrix(c(1,2,3, 0,10, NA), nrow = 3, ncol = 2, byrow = FALSE), "V2", "CC", NA), data.frame(V1 = c(1, 2), V2 = c(0, 10)))` |
| "the input is a data frame, method is MIP and missing_val_char is a qeustion mark" | `expect_equal(impute_missing(data.frame(x = c(1, 2, 3), y = c(0, 10, "?")), "y", "MIP", "?"), data.frame(x = c(1, 2, 3), y = c(0, 10, 5)))` |
| "the input is a matrix, method is MIP and missing_val_char is NaN" | `expect_equal(impute_missing(matrix(c(1,2,3, 0,10,NaN), nrow = 3, ncol = 2, byrow = FALSE), "V2", "MIP", NaN), data.frame(V1 = c(1, 2, 3), V2 = c(0, 10, 5)))` |
| "the input is a data frame, method is DIP and missing_val_char is a blank space" | `expect_equal(impute_missing(data.frame(ex = c(1, 2, 3), bf = c(6, 8, "")), "bf", "DIP", ""), data.frame(ex = c(1, 2, 3), bf = c(6, 8, 7)))` |
| "the input is a matrix, method is DIP and missing_val_char is NA" | `expect_equal(impute_missing(matrix(c(1,2,3, 6,8,NA), nrow = 3, ncol = 2, byrow = FALSE), "V2", "DIP", NA), data.frame(V1 = c(1, 2, 3), V2 = c(6, 8, 7)))` |
