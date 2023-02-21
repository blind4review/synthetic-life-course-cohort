function(analysisOptions=NULL){
  f <- function(d){
    if("useOne" %in% names(analysisOptions)){
      d <- d %>%
        group_by(older) %>%
        dplyr::mutate(groupID=row_number()) %>%
        filter(groupID==1)
    }

    model <- lm(Y~V+C,d)
    est <- model$coefficients[["V"]]
    modsum <- summary(model)
    ci <- confint(model)["V",]
    return(list(est=est,
                se=modsum$coefficients["V","Std. Error"],
                ci_lb=ci[[1]],
                ci_ub=ci[[2]],
                resid_sd=sd(model$residuals)))
    }
  return(f)
}