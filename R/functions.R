### FUNCTIONS


### function to create a directory if it does not exist
### returns the string of the directory
dir_ <- function(name_dir){
  if(!dir.exists(name_dir)) dir.create(name_dir)
  return(name_dir)
}

#define function for R2
r.squared <- function(obs,pred){
  obs = as.numeric(obs)
  pred = as.numeric(pred)
  num = sum((obs-mean(obs))*(pred-mean(pred)))
  den = sqrt(sum((obs-mean(obs))**2))*sqrt(sum((pred-mean(pred))**2))
  rsq = (num/den)**2
  return(rsq)
}

MSE <- function(obs,pred){
  obs = as.numeric(obs)
  pred = as.numeric(pred)
  SE = abs(pred-obs)/obs
  MSE = mean(SE,na.rm=TRUE)
  return(MSE)
}
