# hamr

Handle All Missing (Values) 

*Feb 8th, 2018*

## Project contributors:

1. [Duong Vu](https://github.com/DuongVu39)
2. [Jordan Dubchak](https://github.com/jdubchak)
3. [Linsey Yao](https://github.com/yllz)

### Introduction

Our package intends to explore the pattern of missing values in users' dataset and also imputes the missing values using several methods. 

We decided to make this project because we have not found any package that handle both tasks in either R or Python. In R, we found *[Amelia](https://cran.r-project.org/web/packages/Amelia/Amelia.pdf)* and *[vis_dat](https://cran.r-project.org/web/packages/visdat/visdat.pdf)* package that only visualize the missing data. In Python we found *[fancyimpute](https://pypi.python.org/pypi/fancyimpute)* that deals with missing value but does not have any visualization, and *[missingno](https://github.com/ResidentMario/missingno)* that visualizes missing data. We thought this would be better package for users who do not have much experience in data wrangling.

### To install please execute the following in `R`:

`devtools::install_github("hamr")`

### How to use:

Usage: `vis_missing(x)`  
Input:   
Output: 

Example:

```

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
df <- data.frame(exp = c(1, 2, 3), res = c(0, 10, "")
impute_missing(df, "res", "MIP", "")
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
compare_model(data.frame(ex = c(1, 2, 3), bf = c(6, 8, "")), "bf", "DIP", "")
```


## Functions

Currently, our package only handles continuous features.

- Exploratory Function: use ggplot2/matplotlib/seaborn to plot patterns or proportions of missing values in the dataset:
- `vis_missing()`: A heatmap that visualizes the missing values in the data set.
    Input:
        - dataset
        - ggplot2 color scheme
        - missing value character (NA, "", "?")
- `compare_model()`: Compare summary statistics between various imputation methods
    - Input: 
      - original dataset containing missing values 
      - methods that users want to compare
    - Output: a summary table
    - Call the above function for several methods
    - Compare the summary statistics of what being imputed in the dataset using several available methods

## HAM in Python 
This package is also available in [Python](https://github.com/UBC-MDS/HAM_Python). 

