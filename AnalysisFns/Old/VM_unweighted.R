function(analysisOptions=NULL){
  library("lmtest")
  f <- function(d){
    #Run mixed model
    d$older <- as.factor(d$older)
    model <- lm.cluster(data=d,formula=Y~V+M,cluster="older")

    modsum <- summary(model)
    ci <- confint(model)

     return(list(est=modsum["V","Estimate"],
                 se=modsum["V","Std. Error"],
                 ci_lb=ci[[1]],
                 ci_ub=ci[[2]],
                 resid_sd=sd(model$residuals)))  }
  return(f)
}