suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyverse)
})

#' Produces a visualization of all missing values in a data frame.
#' The missing values are encoded by the missing value character, which is NA by default.
#
#' @title Visualize missing data
#' @param df Input data frame or matrix
#' @param colour Colour scheme for plotting
#' @param missing_val_char the missing value character in the data frame, one of NA, "", " ", "?"
#' @return Visualization of the missing data in a data set
#' @author Jordan Dubchak, March 2018
#' @export
#'
#' @examples
#' vis_missing(df)

vis_missing <- function(df, colour="default", missing_val_char=NA) {
  ## convert input to data frame if not already
  tryCatch({
    todf <- function(dfm) {
      if (!is.data.frame(dfm) & !is.matrix(dfm)) {
        stop("Error: data format is not supported, expected a data frame or a matrix")
      }

      if (!is.data.frame(dfm)) {
        return(as.data.frame(dfm))
      }
      else {
        return(dfm)
      }
    }
    df <- todf(df)
  })

  ## colour argument currently not working

  ## check input of missing value character
  if (!missing_val_char %in% c(NA, "?", " ", "")){
    stop("Error: Missing Value Character not supported. Expected one of: NA, '?', '', ' '")
  }
  ## convert NA values to 1s and all other values to 2 for plotting
  binary <- ifelse(is.na(df), 1, 2)
  ## reshape for plotting
  df_binary <- reshape2::melt(binary)


  ggplot(df_binary, aes(Var2, Var1)) +
    geom_tile(aes(fill=factor(value))) +
    labs(x="", y="", colour="") +
    guides(fill=guide_legend(title=NULL)) +
    scale_fill_discrete(breaks=c(1,2), labels=c("Missing\nValue", "Not Missing\nValue")) +
    theme_bw() +
    ## https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot
    ## https://stackoverflow.com/questions/10861773/remove-grid-background-color-and-top-and-right-borders-from-ggplot2
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.ticks.x= element_blank(), panel.grid.major = element_blank(),
          panel.border = element_blank(), panel.grid.minor = element_blank())
}


#' Impute missing values in a specified column of a data frame or a numerical matrix with three simple methods
#'
#' @param dfm A data frame or a numerical matrix
#' @param col A string of column name, if the input data is a matrix, this should be a string like "Vn" where n is an integer representing the index of column
#' @param method A string of a method name, should be one of "CC", "MIP" and "DIP"
#' @param missing_val_char A string of a missing value format, should be one of NA, NaN, "" and "?"
#' @return A data frame having no missing values in the specified column
#' @author Longlingzi Yao, March 2018
#' @export
#'
#' @examples
#' impute_missing(data.frame(ex = c(1, 2, 3), bf = c(6, 8, "")), "bf", "DIP", "")
#' impute_missing(matrix(c(1,2,3, 6,8,NA), nrow = 3, ncol = 2, byrow = FALSE), "V2", "DIP", NA)
#'
#' @family aggregate functions
#' @seealso \code{\link{na.omit}} for the complete case

impute_missing <- function(dfm, col, method, missing_val_char) {

  '%ni%' <- Negate('%in%')

  if (is.matrix(dfm) & is.na(missing_val_char) == FALSE & is.nan(missing_val_char) == FALSE) {
    stop("Error: only NA and NaN are allowed for matrix, otherwise the input matrix is not numerical")
  }

  if (is.character(col) == FALSE) {
    stop("Error: column name is not applicable, expected a string instead")
  }

  if (method %ni% c("CC", "MIP", "DIP")) {
    stop("Error: method is not applicable")
  }

  if (is.na(missing_val_char) == FALSE & is.nan(missing_val_char) == FALSE & missing_val_char %ni% c("", " ", "?")) {
    stop("Error: missing value format is not supported, expected one of blank space, a question mark, NA and NaN")
  }

  # Pre-process function
  todf <- function(dfm) {
    if (!is.data.frame(dfm) & !is.matrix(dfm)) {
      stop("Error: data format is not supported, expected a data frame or a matrix")
    }

    if (!is.data.frame(dfm)) {
      return(as.data.frame(dfm))
    }
    else {
      return(dfm)
    }
  }

  dfm = todf(dfm)

  if (col %ni% colnames(dfm)) {
    stop("Error: the specified column name is not in the data frame")
  }

  tryCatch({

    if (method == "CC") {
      if (is.na(missing_val_char)) {
        vec <- dfm[,col]
        dfm = dfm[!is.na(vec),]
      }

      else if (is.nan(missing_val_char) | missing_val_char %in% c("", " ", "?")) {
        vec <- dfm[,col]
        vec[is.nan(vec)] <- NA
        vec[vec == ""]  <- NA
        vec[vec == " "]  <- NA
        vec[vec == "?"]  <- NA
        dfm[[col]] <- vec
        dfm = dfm[!is.na(vec),]
      }
    }

    else if (method == "MIP") {
      if (is.na(missing_val_char)) {
        vec <- dfm[,col]
        vec[is.na(vec)] <- mean(vec, na.rm = TRUE)
        dfm[[col]] <- vec
      }
      else if (is.nan(missing_val_char) | missing_val_char %in% c("", " ", "?")) {
        vec <- dfm[,col]
        vec[is.nan(vec)] <- NA
        vec[vec == ""]  <- NA
        vec[vec == " "]  <- NA
        vec[vec == "?"]  <- NA
        vec <- as.numeric(as.character(vec))
        vec[is.na(vec)] <- mean(vec, na.rm = TRUE)
        dfm[[col]] <- vec
      }
    }

    else if (method == "DIP") {
      if (is.na(missing_val_char)) {
        vec <- dfm[,col]
        vec[is.na(vec)] <- median(vec, na.rm = TRUE)
        dfm[[col]] <- vec
      }
      else if (is.nan(missing_val_char) | missing_val_char %in% c("", " ", "?")) {
        vec <- dfm[,col]
        vec[is.nan(vec)] <- NA
        vec[vec == ""]  <- NA
        vec[vec == " "]  <- NA
        vec[vec == "?"]  <- NA
        vec <- as.numeric(as.character(vec))
        vec[is.na(vec)] <- median(vec, na.rm = TRUE)
        dfm[[col]] <- vec
      }
    }

    return(dfm)}, error = function(e) {
      stop("Error: Something unknown went wrong in impute_missing")})
}

#' @title Compare summary statistics between various imputation methods
#' @description This function will call function `impute_missing()` for several methods and
#' return a table with some statistical information of the specified feature
#' before and after imputation of different methods
#' @param df(ndarray) A dataset with missing values that needs to be imputed.
#' @param feature (str) A string of column name, if the input data is a matrix, this should be a string like "Vn" where n is an integer representing the index of column
#' @param methods (str or list)-- the methods that users want to compare
#'                 Supporting methods are:
#'                     CC 	- Complete Case
#'                     MIP - Imputation with mean value
#'                     DIP - Imputation with median value
#' @param missing_val_char A string of a missing value format:
#' Supporting types are:
#'      NaN - Not a Number
#'      "" - Blank
#'      "?" - Question mark
#' @return a summary table comparing the summary statistics: count, mean, std, min, 25\%, 50\%, 75\%, max.
#' @export
#' @author Duong Vu, Master of Data Science, University of British Columbia
#' @examples
#' compare_model(data.frame(ex = c(1, 2, 3), bf = c(6, 8, "")), "bf", "DIP", "")
#' compare_model(matrix(c(1,2,3, 6,8,NA), nrow = 3, ncol = 2, byrow = FALSE), "V2", "DIP", NA)
#'
#' @family aggregate functions
#' @seealso \code{\link{na.omit}} for the complete case

compare_model <- function(df, feature, methods, missing_val_char){

  if (!is.character(feature)) {
    stop("Error: column name is not applicable, expected a string instead")
  }

  if (!feature %in% colnames(df)) {
    stop("Error: the specified column name is not in the data frame")
  }

  if (!method %in% c("CC", "MIP", "DIP")) {
    stop("Error: method is not applicable")
  }

  if (!missing_val_char %in% c(NA, NaN, "?", " ", "")) {
    stop("Error: missing value format is not supported, expected one of blank space, a question mark, NA and NaN")
  }

  result <- broom::tidy(df) %>%
    select(column,mean,sd,min,median,max) %>%
    filter(column == feature)
  methods = c("CC","IMP")

  for(method in meds){
    df_after <- impute_missing(df,feature,method,missing_val_char)
    name <- paste(feature,'_after_', method, sep="")
    b <- broom::tidy(df_after) %>%
      select(column,mean,sd,min,median,max) %>%
      filter(column == feature)
    b$column[1] <- name
    result <- rbind(result,b)
  }

  return (result)

}
