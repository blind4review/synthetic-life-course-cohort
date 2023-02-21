function(analysisOptions=NULL){
  analysisOptions <- analysisOptions
  #   DEBUG
  # instructions <- dget(file.path("Instructions","Dev_Instructions_dag1.R"))
  # d <- read.csv("temp_data.csv")
  # d_younger <- read.csv("temp_younger.csv")
  # analysisOptions <- instructions$analysis$lm_cor_V$options

  f <- function(d,d_younger){
    if("useOne" %in% names(analysisOptions)){
      if(analysisOptions$useOne == TRUE){
        d <- d %>%
          group_by(older) %>%
          dplyr::mutate(groupID=row_number()) %>%
          filter(groupID==1) %>%
          select(-groupID)
      }
    }

    if("exposure" %in% names(analysisOptions)){
      exposureVar <-analysisOptions$exposure
    }else{
      exposureVar <- "V"
    }

    weights <- log(d$weight)-min(log(d$weight))+0.000001

    if("trimWeights" %in%  names(analysisOptions)){
      if(analysisOptions$trimWeights==TRUE){
        #Trim weights
        cutoff <- quantile(weights,c(0.99))
        weights[weights>cutoff] <- cutoff
      }
    }
    if("weighted" %in%  names(analysisOptions)){
      if(analysisOptions$weighted==TRUE){
        model <- lm(as.formula(analysisOptions$formula),d,weights=weights)
      }else{
        model <- lm(as.formula(analysisOptions$formula),d)
      }
    }else{
      model <- lm(as.formula(analysisOptions$formula),d)
    }

    est <- model$coefficients[[exposureVar]]
    ci <- confint(model)[exposureVar,]
    modsum <- summary(model)
    se <- modsum$coefficients[exposureVar,"Std. Error"]
    return(list(est=est,
                se=se,
                ci_lb=ci[[1]],
                ci_ub=ci[[2]],
                resid_sd=sd(model$residuals)))
    }
  return(f)
}