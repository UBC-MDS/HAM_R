\name{compare_model}
\alias{compare_model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compare summary statistics between various imputation methods
%%  ~~function to do ... ~~
}
\description{

This function will call function `impute_missing()` for several methods and
return a table with some statistical information of the specified feature 
before and after imputation of different methods

}
\usage{
compare_model(feature,methods)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{feature}{
  An ndarray, a vector or matrix, of a specified feature from the original     dataset containing missing values that needs to be imputed.
}
  \item{methods}{
  A string or a list specified the methods that users want to compare (default: c("CC","IMP"))
  Supporting methods are: 
    CC 	- Complete Case
    IMP - Imputation with mean value
    KNN - Using KNN to impute the missing value
}
}

\value{
Returns a summary table comparing the summary statistics: count, mean, std, min, 25\%, 50\%, 75\%, max.
}

\author{Duong Vu, Master of Data Science, University of British Columbia}

\examples{

}


