context("Testing the visualization of missing data")

df <- read_csv("../missing_valuesdf.csv") %>% select(-X1)

g <- vis_missing(df, missing_val_char = NA)

p <- vis_missing(df, missing_val_char="?")

p2 <- vis_missing(df, missing_val_char = "")

p3 <- vis_missing(df, missing_val_char = " ")


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
	
})
