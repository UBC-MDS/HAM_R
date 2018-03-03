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
})
