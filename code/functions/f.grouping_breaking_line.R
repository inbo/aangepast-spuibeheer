grouping_breaking_line <- function(x) {
  g_num <- 1
  return_vect <- vector(mode='double',length=length(x))
  for(i in 1:length(x)) {
    if (is.na(x[i])){
      return_vect[i] <- NA
      g_num <- g_num+1
    }
    else {
      return_vect[i] <- g_num
    }
  }
  return(return_vect)
}