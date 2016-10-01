

CalculateRadjusted <- function(model, dt){
  y <- dt[,2,with=FALSE]
  X <- dt[,-2, with=FALSE]
  n <- length(y$nuvirdi)
  p <- length(coef(model))
  residuals <- predict.lm(model,X)-y
  y_mean <- mean(y$nuvirdi)
  SStot <- sum((y$nuvirdi-y_mean)^2)
  SSres <- (sum(residuals$nuvirdi^2))
  Rsquared <- 1-SSres/SStot
  Radjusted <- 1-((1-Rsquared)*(n-1))/(n-p-1)
  return(Radjusted)
}

UltimateFunction <- function(dt){
  
}