function(analysisOptions=NULL){
  library("lmtest")
  library("miceadds")
  analysisOptions <- analysisOptions
  f <- function(d, d_younger){
    if("useOne" %in% names(analysisOptions)){
      if(analysisOptions$useOne==TRUE){
        d <- d %>%
          group_by(older) %>%
          dplyr::mutate(groupID=row_number()) %>%
          filter(groupID==1) %>%
          select(-groupID)
      }
    }
    weights <- log(d$weight)-min(log(d$weight))+0.000001

    if("trimWeights" %in%  names(analysisOptions)){
      if(analysisOptions$trimWeights==TRUE){
        #Trim weights
        cutoff <- quantile(weights,c(0.99))
        weights[weights>cutoff] <- cutoff
      }
    }

    #Run cluster model
    if(analysisOptions$weighted==TRUE){
      d$older <- as.factor(d$older)
      model <- lm.cluster(data=d,
                          weights=weights,
                          formula=as.formula(analysisOptions$formula),
                          cluster=analysisOptions$clusterVar)

    }else{
      d$older <- as.factor(d$older)
      model <- lm.cluster(data=d,
                          formula=as.formula(analysisOptions$formula),
                          cluster=analysisOptions$clusterVar)
    }

    modsum <- summary(model)
    ci <- confint(model)

    if("exposure" %in% names(analysisOptions)){
      exposureVar <- analysisOptions$exposure
    }else{
      exposureVar <- "V"
    }

    est <- modsum[exposureVar,"Estimate"]

    return(list(est=est,
                se=modsum[exposureVar,"Std. Error"],
                ci_lb=ci[[1]],
                ci_ub=ci[[2]],
                resid_sd=sd(model$residuals)))  }
  return(f)
}