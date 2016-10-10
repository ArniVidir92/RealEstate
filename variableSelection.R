findBestBICModel <- function(null, full, dTr, dTe, lambda){
  lm.BIC <- step(null, scope = list(lower=null,upper=full), direction="both", k=log(dim(dTr)[1]))
  Radj.BICStep <- CalculateRadjLambda(lm.BIC, dTe, lambda) # 0.8889675 rétt: 0.8352896
  return(list(model=lm.BIC, Radj=Radj.BICStep))
}

findBestAICModel <- function(null, full, dTr, dTe, lambda){
  lm.AIC <- step(null, scope = list(lower=null,upper=full), direction="both", k=2)
  Radj.AICStep <- CalculateRadjLambda(lm.AIC, dTe, lambda) # 0.889269 # rétt: 0.8360234
  return(list(model=lm.AIC, Radj=Radj.AICStep))
}