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

impute_missing <- function(dfm, col, method, missing_val_char) {
  
  dfm = todf(dfm)

  '%ni%' <- Negate('%in%')
  
  if (is.character(col) == FALSE) {
    stop("Error: column name is not applicable, expected a string instead")
  }
  
  if (col %ni% colnames(dfm)) {
    stop("Error: the specified column name is not in the data frame")
  }

  if (method %ni% c("CC", "MIP", "DIP")) {
    stop("Error: method is not applicable")
  }

  if (is.na(missing_val_char) == FALSE & is.nan(missing_val_char) == FALSE & missing_val_char %ni% c("", "?")) {
    stop("Error: missing value format is not supported, expected one of blank space, "?", NA or NaN")
  }

  tryCatch({

    if (method == "CC") {
      if (is.na(missing_val_char)) {
        dfm = na.omit(dfm)
      }
      
      else if (is.nan(missing_val_char) | missing_val_char %in% c("", "?")) {
        vec <- dfm[,col]
        vec[is.nan(vec)] <- NA
        vec[vec == ""]  <- NA
        vec[vec == "?"]  <- NA
        dfm[[col]] <- vec
        dfm = na.omit(dfm)
      }
    }
    
    else if (method == "MIP") {
      if (is.na(missing_val_char)) {
        vec <- dfm[,col]
        vec[is.na(vec)] <- mean(vec, na.rm = TRUE)
        dfm[[col]] <- vec
      }
      else if (is.nan(missing_val_char) | missing_val_char %in% c("", "?")) {
        vec <- dfm[,col]
        vec[is.nan(vec)] <- NA
        vec[vec == ""]  <- NA
        vec[vec == "?"]  <- NA
        vec <- as.numeric(as.character(vec))
        vec[is.na(vec)] <- mean(vec, na.rm = TRUE)
        dfm[[col]] <- vec
      }
    }
    
    else if (method == "MIP") {
      if (is.na(missing_val_char)) {
        vec <- dfm[,col]
        vec[is.na(vec)] <- median(vec, na.rm = TRUE)
        dfm[[col]] <- vec
      }
      else if (is.nan(missing_val_char) | missing_val_char %in% c("", "?")) {
        vec <- dfm[,col]
        vec[is.nan(vec)] <- NA
        vec[vec == ""]  <- NA
        vec[vec == "?"]  <- NA
        vec <- as.numeric(as.character(vec))
        vec[is.na(vec)] <- median(vec, na.rm = TRUE)
        dfm[[col]] <- vec
      }
    }
    
    return(dfm)}, error = function(e) {
      stop("Error: Something unknown went wrong in impute_missing")})
}
