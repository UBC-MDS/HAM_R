# hamr

[![Build Status](https://travis-ci.org/UBC-MDS/hamr.svg?branch=master)](https://travis-ci.org/UBC-MDS/hamr)

Handle All Missing (Values) 

## Project contributors:

1. [Duong Vu](https://github.com/DuongVu39)
2. [Jordan Dubchak](https://github.com/jdubchak)
3. [Linsey Yao](https://github.com/yllz)

### Introduction

Our package intends to explore the pattern of missing values in users' dataset and also imputes the missing values using several methods. 

We decided to make this project because we have not found any package that handle both tasks in either R or Python. In R, we found *[Amelia](https://cran.r-project.org/web/packages/Amelia/Amelia.pdf)* and *[vis_dat](https://cran.r-project.org/web/packages/visdat/visdat.pdf)* package that only visualize the missing data. In Python we found *[fancyimpute](https://pypi.python.org/pypi/fancyimpute)* that deals with missing value but does not have any visualization, and *[missingno](https://github.com/ResidentMario/missingno)* that visualizes missing data. We thought this would be better package for users who do not have much experience in data wrangling.

### To install please execute the following in `R`:

`devtools::load_all()`

`devtools::install_github("UBC-MDS/hamr")`

### How to use:

Usage: `vis_missing(dfm, colour="default", missing_val_char = NA)`  
Input: 

- `dfm`: a data frame or matrix containing missing values
- `colour`: a base R or `ggplot2` colour map, defaults to `ggplot2` default
- `missing_val_char`: the character representing missing values in data frame. One of: c(NA, " ", "", "?")

Output: A visualization of missing data across the data frame. 

Example:

```
df <- data.frame(x = c(1, " ", 3), y = c(1, 8, 9))
vis_missing(df, missing_val_char = " ")

```

--

Usage: `impute_missing(dfm, col, method, missing_val_char)`  
Input: 

- `dfm`: a data frame or a matrix with missing values
- `col`: a column name (string)
- `method`: a method name ("CC", "MIP", "DIP")
- `missing_val_char`: missing value characters (NA, NaN, "", "?")

Output: a data frame with no missing values in the specified column

Example:

```
> df <- data.frame(exp = c(1, 2, 3), res = c(0, 10, ""))
> impute_missing(df, "res", "MIP", "")
  exp res
1   1   0
2   2  10
3   3   5
```

--

Usage: `compare_model(df, feature, methods, missing_val_char)`  
Input:   

- `df` (ndarray) -- the original dataset with missing values that needs to be imputed.
  feature (str) -- name of a specified feature from the original dataset containing missing values that need to be imputed.

- `methods` (str or list) -- the methods that users want to compare (default: ["CC","IMP"])

  - Supporting methods are: 

  ​            CC   - Complete Case
  ​            MIP - Imputation with mean value
  ​            DIP  - Imputation with median value

- `missing_val_char` (str) -- missing value types. 

  - Supporting types are:

  ​            NaN - Not a Number
  ​            ""      - Blank
  ​            "?"     - Question mark

Output:  a summary table comparing the summary statistics: count, mean, std, min, 25%, 50%, 75%, max.

Example:

```python
> df <- data.frame(exp = c(1, 2, 3), res = c(0, 10, ""))
> compare_model(df, "res", c("CC","MIP"), "")
         column mean       sd min median max
2  res_after_CC    5 7.071068   0      5  10
3 res_after_MIP    5 5.000000   0      5  10
```

--

## HAM in Python 

This package is also available in [Python](https://github.com/UBC-MDS/HAM_Python). 

