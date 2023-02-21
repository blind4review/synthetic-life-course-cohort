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
    correction_factor <- NA
    sig_sq<-NA
    omega_sq<-NA
    uncorrected_est <- est
    uncorrected_ci <- ci
    if("measErrorCorrection" %in% names(analysisOptions)){
      if(analysisOptions$measErrorCorrection==TRUE){
        # Step 1) Calculate the population variance of V
        sig_sq <- sd(d_younger$V)^2 #x is distributed with mean mu and sd sigma^2

        #Step 2) Estimate variance in the measurement error (V_younger given true V_older)
        #  measFormula e.g.: V~M
        #  V_pred is the estimate of the true V_older
        # m_V <- lm(analysisOptions$measFormula, d_younger)
        # d_younger$V_pred <- predict(m_V) #+ rnorm(nrow(d_younger),
        #                                  #         mean=0,
        #                                  #         sd=sd(residuals(m_V))/sqrt(nrow(d_younger)))
        #
        # m_meas <- lm(V_pred~V, d_younger)
        # omega_sq <- sd(residuals(m_meas))^2 #z|x is distributed with mean x and sd omega^2
        m_V <- lm(analysisOptions$measFormula,
                  d_younger)
        omega_sq <- sd(residuals(m_V))^2

        # Step 3) Calculate the correction factor
        correction_factor <- sig_sq/(sig_sq+omega_sq)

        # Step 5) Correct for measurement error
        est <- est/correction_factor

        # Step 6) Adjust CI
        #TO DO
      }
    }

    return(list(correction_factor=correction_factor,
                sig_sq=sig_sq,
                omega_sq=omega_sq,
                uncorrected_est=uncorrected_est,
                est=est,
                se=modsum[exposureVar,"Std. Error"],
                ci_lb=ci[[1]],
                ci_ub=ci[[2]],
                uncorrected_ci_lb = uncorrected_ci[[1]],
                uncorrected_ci_ub = uncorrected_ci[[2]],
                resid_sd=sd(model$residuals)))  }
  return(f)
}