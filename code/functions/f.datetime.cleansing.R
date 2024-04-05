datetime_cleansing<-function(data,col1,col2,format){
  data[,col2] <- apply( data[ , c(col1,col2) ] , 1 , paste , collapse = " " )
  data <- data[ , !( names( data ) %in% c(col1) ) ]
  data[,col2] <- as.POSIXct(data[,col2], format = format)
  return(data)
}