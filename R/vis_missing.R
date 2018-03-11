#' Produces a visualization of all missing values in a data frame.
#' The missing values are encoded by the missing val character, which is NA by default.
#
#' @param df Input data frame or matrix
#' @param colour Colour scheme for plotting
#' @param missing_val_char The missing value character in the data frame, one of NA, "", " ", "?"  
#' @examples 
#' vis_missing(df)

suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyverse)
})

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

vis_missing <- function(df, colour="default", missing_val_char=NA){
  ## convert input to data frame if not already 
  tryCatch({
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


