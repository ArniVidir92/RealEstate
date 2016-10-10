source("testArni.R")

CalculateRadjLambda <- function(model, dt, lambda){
  y <- dt[,2,with=FALSE]
  X <- dt[,-2, with=FALSE]
  n <- length(y$nuvirdi)
  p <- length(coef(model))
  pred <- (predict.lm(model,X)*lambda + 1)^(1/lambda)
  y <- (y$nuvirdi*lambda + 1)^(1/lambda)
  residuals <- pred-y
  y_mean <- mean(y)
  SStot <- sum((y-y_mean)^2)
  SSres <- (sum(residuals^2))
  Rsquared <- 1-SSres/SStot
  Radjusted <- 1-((1-Rsquared)*(n-1))/(n-p-1)
  return(Radjusted)
}

##' @title bcTransF
##' @param dt DataTable that includes the column nuvirdi
##' @param lambda Is the x value of the highest point in a boxcox plot for lm(nuvirdi ~ ., data=dt)
##' @return dt with the nuvirdi column transformed
##' @author Árni og Ottó
bcTransF <- function(dt, lambda){
  dt$nuvirdi <- yjPower(dt$nuvirdi, lambda)
  return(dt)
}

BCTranformResponseRadj <- function(model, dTr, dTe){
  bc <- boxcox(model, plotit = FALSE)
  lambda <- with(bc, x[which.max(y)])
  trainBC <- bcTransF(dTr, lambda)
  testBC <- bcTransF(dTe, lambda)
  lm.temp <- lm(nuvirdi ~ ., data = trainBC)
  return(CalculateRadjLambda(lm.temp, testBC, lambda))
}

TransformBCandIBM2 <- function(model, dTr, dTe){
  lambda <- with(boxcox(model, plotit = FALSE), x[which.max(y)])
  trainBC <- bcTransF(dTr, lambda )
  testBC <- bcTransF(dTe, lambda )
  trainBC$ibm2 <- log(trainBC$ibm2)
  testBC$ibm2 <- log(testBC$ibm2)
  lm.temp <- lm(nuvirdi ~ ., data = trainBC)
  return(CalculateRadjLambda(lm.temp, testBC, lambda))
}

GetBCandIBM2ModelAndDt <- function(model, dTr, dTe){
  lambda <- with(boxcox(model, plotit = FALSE), x[which.max(y)])
  trainBC <- bcTransF(dTr, lambda )
  testBC <- bcTransF(dTe, lambda )
  trainBC$ibm2 <- log(trainBC$ibm2)
  testBC$ibm2 <- log(testBC$ibm2)
  lm.temp <- lm(nuvirdi ~ ., data = trainBC)
  return(list(model=lm.temp, train=trainBC, test=testBC, lambda=lambda))
}
