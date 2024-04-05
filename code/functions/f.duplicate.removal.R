duplicate_removal<-function(data){
  data <- data[order(data[,"Tijd"]),]
  data <- data[!duplicated(data[,"Tijd"]),]
  return(data)
}