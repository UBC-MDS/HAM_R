context("Testing compare_model")
library(tidyverse)
df <- read.csv("../dummy_dataset.csv")

meds <- c("CC","IMP")
result <- compare_model('col1', methods = meds)

test <- broom::tidy(df) %>% 
  select(column,mean,sd,min,median,max) %>% 
  filter(column == "col1")

for(method in meds){
  df_after <- impute_missing('col1',method)
  name <- paste('col1_after_', method, sep="")
  b <- broom::tidy(df_after) %>% 
    select(column,mean,sd,min,median,max) %>%
    filter(column == "col1")
  b['col1'] <- name
  test <- rbind(test,b)
}

test_that("compare_model(feature, method),
          Test this function for a normal case", {
    expect_match(typeof(result),"list")
    expect_match(class(result),"data.frame")
    expect_equal(test,result)
    
    # expected errors
    expect_error(compare_model(list(1, 2, 2, NA), "V2", "MIP", "NA"),
                 "Error: data format is not supported, expected a data frame or a matrix")
    
    expect_error(compare_model(data.frame("exp" = c(1, 2, 3), "res" = c(0, 10, "NaN")),
                                2,
                                "CC",
                                NaN),
                 "Error: column name is not applicable, expected a string instead")
    
    expect_error(compare_model(data.frame(exp = c(1, 2, 3), res = c(0, 10, "")),
                                "dn",
                                "CC",
                                ""),
                 "Error: the specified column name is not in the data frame")
    
    expect_error(compare_model(data.frame(x = c(1, 2, 3), y = c(0, 10, NaN)), "y", "multi_im", "NaN"),
                 "method is not applicable")
    
    expect_error(compare_model(data.frame(x = c(1, 2, 3), y = c(0, 10, 0)), "y", "CC", 0),
                 "Error: missing value format is not supported, expected one of blank space, a question mark, NA and NaN")
})


