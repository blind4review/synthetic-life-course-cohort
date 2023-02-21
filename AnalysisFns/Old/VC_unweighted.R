function(analysisOptions=NULL){
  library("lmtest")
  f <- function(d){
    if("useOne" %in% names(analysisOptions)){
      d <- d %>%
        group_by(older) %>%
        dplyr::mutate(groupID=row_number()) %>%
        filter(groupID==1)
    }

    #Run mixed model
    d$older <- as.factor(d$older)
    model <- lm.cluster(data=d,formula=Y~V+C,cluster="older")

    modsum <- summary(model)
    ci <- confint(model)

     return(list(est=modsum["V","Estimate"],
                 se=modsum["V","Std. Error"],
                 ci_lb=ci[[1]],
                 ci_ub=ci[[2]],
                 resid_sd=sd(model$residuals)))
  }
  return(f)
}