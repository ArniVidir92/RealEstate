if(!exists("dt.RE_data")){ source("setUp.R")}

diag <- data.table(fortify(lm.all))
res_sd <- sd(diag$.resid)
res_index <- ggplot(diag, aes(x=seq(1:length(.resid)),y=.resid)) + geom_point()
res_index <- res_index + geom_hline(yintercept=0, col="red", linetype="dashed")
res_index <- res_index + geom_hline(yintercept=2*res_sd, col="blue", linetype="dashed")
res_index <- res_index + geom_hline(yintercept=-2*res_sd, col="blue", linetype="dashed")
res_index <- res_index + geom_hline(yintercept=3*res_sd, col="green", linetype="dashed")
res_index <- res_index + geom_hline(yintercept=-3*res_sd, col="green", linetype="dashed")
res_index <- res_index + xlab("Index")+ylab("Residuals")+labs(title = "Index plot of residuals")
res_index <- res_index + geom_text(aes(label=ifelse(abs(.resid)>2*res_sd,nuvirdi,"")),hjust=-.5, vjust=0.4)

res_index

sort(diag$nuvirdi)

Jack_index<-ggplot(diag, aes(x=seq(1:length(.jack)),y=.jack))+geom_point()
Jack_index<-Jack_index+geom_hline(yintercept=0, col="red", linetype="dashed")
Jack_index<-Jack_index+xlab("Index")+ylab("Jackknife residuals")
Jack_index<-Jack_index+geom_text(aes(label=ifelse(abs(.jack)>2.4,.index,"")),hjust=0, vjust=0)
Jack_index

indexPlotLeverage <- function(model){
  diag <- data.table(fortify(model))
  diag$.index = c(1:length(diag$.resid))
  p<-length(coef(model))
  n<-length(fitted(model))
  lev_index<-ggplot(diag, aes(x=seq(1:length(.hat)),y=.hat))+geom_point()
  lev_index<-lev_index+geom_hline(yintercept=2*p/n, col="red", linetype="dashed")
  lev_index<-lev_index+xlab("Index")+ylab("Leverages")
  lev_index<-lev_index+geom_text(aes(label=ifelse(.hat>2*p/n,.index,"")),hjust=0, vjust=0)
  lev_index
}

# blue dots have high residuals and green have high leverage
indexPlotCookdistance <- function(model){
  diag <- data.table(fortify(model))
  diag$.index = c(1:length(diag$.resid))
  p <- length(coef(model))
  n <- length(fitted(model))
  cook_index <- ggplot(diag, aes(x=seq(1:length(.cooksd)),y=.cooksd)) 
  cook_index <- cook_index + geom_point( aes(color = ifelse(abs(.hat)>2*p/n,"blue",ifelse(abs(.stdresid)>2,"red","black"))))
  cook_index <- cook_index + xlab("Index")+ylab("Cooks distance") + theme(legend.position="none")
  cook_index <- cook_index + geom_text(aes(label=ifelse(abs(.cooksd)>0.1 | abs(.hat)>2*p/n | abs(.stdresid)>2,.index,"")),hjust=0, vjust=0)
  cook_index
}

indexPlotResiduals <- function(model){
  diag <- data.table(fortify(model))
  diag$.index = c(1:length(diag$.resid))
  res_sd <- sd(diag$.resid)
  res_index <- ggplot(diag, aes(x=seq(1:length(.resid)),y=.resid)) + geom_point()
  res_index <- res_index + geom_hline(yintercept=0, col="red", linetype="dashed")
  res_index <- res_index + geom_hline(yintercept=2*res_sd, col="blue", linetype="dashed")
  res_index <- res_index + geom_hline(yintercept=-2*res_sd, col="blue", linetype="dashed")
  res_index <- res_index + geom_hline(yintercept=3*res_sd, col="green", linetype="dashed")
  res_index <- res_index + geom_hline(yintercept=-3*res_sd, col="green", linetype="dashed")
  res_index <- res_index + xlab("Index")+ylab("Residuals")+labs(title = "Index plot of residuals")
  res_index <- res_index + geom_text(aes(label=ifelse(abs(.resid)>2*res_sd,.index,"")),hjust=-.5, vjust=0.4)
  res_index
}

indexPlotStResiduals <- function(model){
  diag <- data.table(fortify(model))
  diag$.index = c(1:length(diag$.resid))
  Stres_sd <- sd(diag$.stdresid)
  Stres_index<-ggplot(diag, aes(x=seq(1:length(.stdresid)),y=.stdresid))+geom_point()
  Stres_index<-Stres_index+geom_hline(yintercept=0, col="red", linetype="dashed")
  Stres_index <- Stres_index + geom_hline(yintercept=2*Stres_sd, col="blue", linetype="dashed")
  Stres_index <- Stres_index + geom_hline(yintercept=-2*Stres_sd, col="blue", linetype="dashed")
  Stres_index <- Stres_index + geom_hline(yintercept=3*Stres_sd, col="green", linetype="dashed")
  Stres_index <- Stres_index + geom_hline(yintercept=-3*Stres_sd, col="green", linetype="dashed")
  Stres_index <- Stres_index+xlab("Index")+ylab("Studentized residuals")+labs(title = "Index plot of std. residuals")
  Stres_index <- Stres_index+geom_text(aes(label=ifelse(abs(.stdresid)>2*Stres_sd,.index,"")),hjust=0, vjust=0)
  Stres_index
}

indexPlotJackResiduals <- function(model){
  diag <- data.table(fortify(model))
  diag$.index = c(1:length(diag$.resid))
  diag$.jack<-rstudent(model)
  Jack_index<-ggplot(diag, aes(x=seq(1:length(.jack)),y=.jack))+geom_point()
  Jack_index<-Jack_index+geom_hline(yintercept=0, col="red", linetype="dashed")
  Jack_index<-Jack_index+xlab("Index")+ylab("Jackknife residuals")
  Jack_index<-Jack_index+geom_text(aes(label=ifelse(abs(.jack)>2.4,.index,"")),hjust=0, vjust=0)
  Jack_index
}


