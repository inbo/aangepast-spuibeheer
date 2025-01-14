f.sp.conductance <- function(conductivity, temperature, alpha=0.02){
  temperature <- c(temperature)
  conductivity <- c(conductivity)
  
  if(length(temperature) != length(conductivity)){
    return("Lengths do not match!")
  }
  
  specific_conductance <- conductivity/(1 + alpha * (temperature - 20))

  return(specific_conductance)
}