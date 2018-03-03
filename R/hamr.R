# Storing all function

# JD function

# Linsey function

# Compare_model function
# A summary function that compares summary statistics between various imputation methods
compare_model <- function(feature, methods=c("CC","IMP")){
  

assert feature != None "Missing feature"
assert isinstance(methods, list) or isinstance(methods, str) 
"Input method(s) is not in the right type"
assert isinstance(feature, pd.DataFrame) or isinstance(feature, np.ndarray) 
"Input feature is not in the right type"
}
