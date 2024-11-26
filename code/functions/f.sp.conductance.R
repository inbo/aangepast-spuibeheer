f.sp.conductance <- function(temperature, conductivity){
  temperature <- c(temperature)
  conductivity <- c(conductivity)
  
  if(length(temperature) != length(conductivity)){
    return("Lengths do not match!")
  }
  
  specific_conductance <- conductivity/(1 - ((25 - temperature)*2/100))
  return(specific_conductance)
}