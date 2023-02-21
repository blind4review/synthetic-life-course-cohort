function(analysisOptions=NULL){
  library("lmtest")
  f <- function(d){
    #Trim weights
    cutoff <- quantile(d$weight,c(0.99))
    weights <- d$weight
    weights[weights>cutoff] <- cutoff

    #Run mixed model
    d$older <- as.factor(d$older)
    model <- lm.cluster(data=d,weights=weights,formula=Y~V+M,cluster="older")

    modsum <- summary(model)
    ci <- confint(model)

     return(list(est=modsum["V","Estimate"],
                 se=modsum["V","Std. Error"],
                 ci_lb=ci[[1]],
                 ci_ub=ci[[2]],
                 resid_sd=sd(model$residuals)))  }
  return(f)
}