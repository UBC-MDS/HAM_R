# Pre-process function
todf <- function(dfm, colnames=FALSE) {
  if (!is.data.frame(dfm) & !is.matrix(dfm)) {
    stop("Error: expected a data frame or a matrix")
  }
  
  if (!is.data.frame(dfm)) {
    return(as.data.frame(dfm)) 
  }
  else {
    return(dfm)
  }
}

impute_missing <- function(dfm, col, method, missing_val_char) {

  '%ni%' <- Negate('%in%')

  if (method %ni% c("CC", "MIP", "DIP")) {
    stop("Error: method is not applicable")
  }

  if (is.na(missing_val_char) == FALSE & is.nan(missing_val_char) == FALSE & missing_val_char %ni% c("", "?")) {
    stop("Error: missing value format is not supported, expected one of blank space, "?", NA or NaN")
  }

  tryCatch({
    dfm = todf(dfm)
    
    # Nested conditions
    if (method == "CC") {
      if (is.na(missing_val_char)) {
        dfm = na.omit(dfm)
      }
      else if (is.nan(missing_val_char)) {
        dfm[is.nan(dfm)] <- NA
        dfm = na.omit(dfm)
      }
      else if (missing_val_char %in% c("", "?")) {
        dfm[dfm == ""]  <- NA
        dfm[dfm == "?"]  <- NA
        dfm = na.omit(dfm)
      }
    }
    
    else if (method == "MIP") {
      if (is.na(missing_val_char)) {
        for (i in 1:ncol(dfm)) {
          dfm[is.na(dfm[,i]), i] <- mean(dfm[,i], na.rm = TRUE)
        }
      }
      else if (is.nan(missing_val_char)) {
        dfm[is.nan(dfm)] <- NA
        for (i in 1:ncol(dfm)) {
          dfm[is.na(dfm[,i]), i] <- mean(dfm[,i], na.rm = TRUE)
        }
      }
      else if (missing_val_char %in% c("", "?")) {
        dfm[dfm == ""]  <- NA
        dfm[dfm == "?"]  <- NA
        for (i in 1:ncol(dfm)) {
          dfm[is.na(dfm[,i]), i] <- mean(dfm[,i], na.rm = TRUE)
        }
      }
    }
    
    else if (method == "DIP") {
      if (is.na(missing_val_char)) {
        for (i in 1:ncol(dfm)) {
          dfm[is.na(dfm[,i]), i] <- median(dfm[,i], na.rm = TRUE)
        }
      }
      else if (is.nan(missing_val_char)) {
        dfm[is.nan(dfm)] <- NA
        for (i in 1:ncol(dfm)) {
          dfm[is.na(dfm[,i]), i] <- median(dfm[,i], na.rm = TRUE)
        }
      }
      else if (missing_val_char %in% c("", "?")) {
        dfm[dfm == ""]  <- NA
        dfm[dfm == "?"]  <- NA
        for (i in 1:ncol(dfm)) {
          dfm[is.na(dfm[,i]), i] <- median(dfm[,i], na.rm = TRUE)
        }
      }
    }
    
    return(dfm)}, error = function(e) {
      stop("Error: Something unknown went wrong in impute_missing")})
}
