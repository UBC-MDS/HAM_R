context("Testing the visualization of missing data")

df <- read_csv("missing_valuesdf.csv")

g <- vis_missing(df, missing_val_char = "NA")

test_that("vis_missing(df, colour, missing_val_char) returns a heatmap visualization of the missing values in a data set"), {

	expect_equal(
		, g$coordinates$limits$x == c(50, 0) 
		)

	expect_equal(
		, g$coordinates$limits$y == c(0, 4) 
		)

	expect_equal(
		, is.null(g$layer[[1]]) == FALSE  
		)
}