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

    #Measurement error correction
    correction_factor <- NA
    sig_sq<-NA
    omega_sq<-NA
    uncorrected_est <- est
    uncorrected_ci <- ci
    if("measErrorCorrection" %in% names(analysisOptions)){
       if(analysisOptions$measErrorCorrection==TRUE){
        # Step 1) Calculate the population variance of V
        sig_sq <- sd(d_younger$V)^2

        #Step 2) Estimate variance in the measurement error (V_younger given true V_older)
        m_V <- lm(analysisOptions$measFormula,
                  d_younger)
        m_M <- lm(M_younger~V,
                  d_younger)

        d_pred <- d_younger
        d_pred$V <- rnorm(nrow(d_pred),sd=sd(d_younger$V))
        d_pred$M_younger <- predict(m_M,newdata=d_pred,type="response")+rnorm(nrow(d_younger),sd=sd(residuals(m_M)))
        d_pred$V_pred <- predict(m_V,newdata=d_pred,type="response")+rnorm(nrow(d_younger),sd=sd(residuals(m_V)))

        m_meas <- lm(V_pred~V_younger-1,d_pred)
        #omega_sq <- sd(residuals(m_meas))^2
        #omega_sq <- mean((d_pred$V-d_pred$V_pred)^2)

        omega_sq <- sd(residuals(m_meas))^2
        omega_sq_true <- sd(residuals(lm(V_younger~V_older-1,d)))^2

        # Step 3) Calculate the correction factor
        correction_factor <- sig_sq/(sig_sq+omega_sq)

        # Step 5) Correct for measurement error
        est <- uncorrected_est/correction_factor

        # Step 6) Adjust CI
        #TO DO
       }
    }

    return(list(correction_factor=correction_factor,
                sig_sq=sig_sq,
                omega_sq=omega_sq,
                uncorrected_est=uncorrected_est,
                est=est,
                se=NA,#modsum$coefficients[exposureVar,"Std. Error"],
                ci_lb=ci[[1]],
                ci_ub=ci[[2]],
                uncorrected_ci_lb = uncorrected_ci[[1]],
                uncorrected_ci_ub = uncorrected_ci[[2]],
                resid_sd=sd(model$residuals)))
    }
  return(f)
}