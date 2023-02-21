function(analysisOptions=NULL){
  f <- function(d){
    model <- lm(Y~V+M,d)
    est <- model$coefficients[["V"]]
    modsum <- summary(model)
    return(list(est=est,
                se=modsum$coefficients["V","Std. Error"],
                resid_sd=sd(model$residuals)))
  }
  return(f)
}