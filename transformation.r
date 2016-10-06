bcTransF <- function(dt, lambda){
  dt$nuvirdi <- yjPower(dt$nuvirdi, lambda)
  return(dt)
}

