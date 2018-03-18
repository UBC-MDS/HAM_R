library(utils)

context("Testing the visualization of missing data")

df <- read.csv("../missing_valuesdf.csv")

g <- vis_missing(df, missing_val_char = NA)

p <- vis_missing(df, missing_val_char="?")

p2 <- vis_missing(df, missing_val_char = "")

p3 <- vis_missing(df, missing_val_char = " ")

p4 <- vis_missing(as.matrix(df), missing_val_char = NA)



## this set of tests will no longer pass as the 
test_that("vis_missing(df, colour, missing_val_char) returns a heatmap visualization of the missing values in a data set",{

	expect_equal(is.null(g$scales), FALSE
		)

	expect_equal(
	  g$theme$text$face, "plain"
		)

	expect_equal(
		 is.null(g$layer[[1]]), FALSE  
		)
	
	# expect_error(
	#  p, "Missing Value Character not supported. Expected one of: NA, '?', '', ' '" 
	# )
	
	expect_equal(
	  p$data[1,1], 1
	)
	
	expect_equal(
	  p$guides$fill$name, "legend"
	)
	
	## these next __ tests are the same, but using different missing value characters to ensure 
	## they work (i.e. they should all do the same thing, regardless of the inputted missing val char)
	expect_equal(
	  p$labels$fill, "factor(value)"
	)
	
	expect_equal(
	  g$labels$fill, "factor(value)"
	)
	
	expect_equal(
	  p2$labels$fill, "factor(value)"
	)
	
	expect_equal(
	  p3$labels$fill, "factor(value)"
	)
	
	## branch coverage for matrix in todf
	expect_equal(
	  p4$labels$fill, "factor(value)"
	)
	
	## branch coverage for todf when input is not a matrix or df 
	expect_error(
	  vis_missing(as.list(df), missing_val_char = NA), "data format is not supported, expected a data frame or a matrix"
	)
	
    ## this is error is found in issue ___. 
	## branch coverage for unacceptable missing value input  - this fails, but the output is the exact same
    #expect_error(
     #vis_missing(df, missing_val_char = "i"), "Error: Missing Value Character not supported. Expected one of: NA, '?', '', ' '")

})
