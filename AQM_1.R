get_dataframe <- function(filepath){
  x <- read.csv(filepath)
  df <- as.data.frame(x)
  
  return(df)
}

compute_mean <- function(filepath){
  df <- get_dataframe(filepath)
  cmeans <- colMeans(df)
  return(cmeans)
}


