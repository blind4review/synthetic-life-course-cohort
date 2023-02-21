#TO DO: IMPLEMENT THIS SO THAT WE LOOP OVER THE DETERMINISTIC MATCHES AND CALCULATE
#       WITHIN- AND ACROSS- VARIANCE. ADD THEM TOGETHER. SEE NOTES FROM 9/19/22 DATA POOLING MEETING.

function(analysisOptions=NULL){
  analysisOptions <- analysisOptions
  #   DEBUG
  # instructions <- dget(file.path("Instructions","Dev_Instructions_dag1.R"))
  # d <- read.csv("temp_data.csv")
  # d_younger <- read.csv("temp_younger.csv")
  # analysisOptions <- instructions$analysis$lm_cor_V$options

  f <- function(d,d_younger){
    if("exposure" %in% names(analysisOptions)){
      exposureVar <-analysisOptions$exposure
    }else{
      exposureVar <- "V"
    }

    #Make a data set that looks like MICE's "mice()" output
    # test  <- d %>% group_by(older) %>%  mutate(id = row_number()) %>% filter(id==1)
    # na_indices <- which(rbinom(nrow(test),1,0.05)==1)
    # test[na_indices,"V"] <- NA
    # mice_result <- mice(test,m=5)

    #
    if(round(nrow(d))!=nrow(d)){
      stop("ERROR: inconsistent number of matches")
    }

    nMatches <- nrow(d %>% filter(older==1))
    dataSets <- list()

    #Assign random partition ID number to each matched pair
    d <- d %>% group_by(older) %>% dplyr::mutate(id = sample(1:nMatches)) %>%  ungroup()

    for(i in 1:nMatches){
      dataSets[[i]] <- list(d=d %>% filter(id==i))
      model <- lm(as.formula(analysisOptions$formula),
                  dataSets[[i]]$d)
      dataSets[[i]]$est <- model$coefficients[[exposureVar]]
      dataSets[[i]]$ci <- confint(model)[exposureVar,] #Not using these anywhere - too narrow
      modsum <- summary(model)
      dataSets[[i]]$se <- modsum$coefficients[exposureVar,"Std. Error"]
    }

    estimates <- unlist(purrr::map_depth(dataSets,.depth=1,"est"))
    ses <- unlist(purrr::map_depth(dataSets,.depth=1,"se"))

    mean_est  <- mean(estimates)
    var_within <- mean(ses^2)
    var_across <- (1/(nMatches-1))*(sum((estimates-mean_est)^2))
    var_total <- var_within + (1+1/nMatches)*var_across
    se_total <- sqrt(var_total)

    ci_rubin_prop_covered <- 0
    coverage_min <- 1
    coverage_max <- 0
    for(i in 1:nMatches){
      dataSets[[i]]$ci_rubin_lb <- dataSets[[i]]$est - 1.96*se_total
      dataSets[[i]]$ci_rubin_ub <- dataSets[[i]]$est + 1.96*se_total
      dataSets[[i]]$ci_rubin_covered <- dataSets[[i]]$ci_rubin_lb < truths$TrueBigPop  &
                                        truths$TrueBigPop < dataSets[[i]]$ci_rubin_ub
      ci_rubin_prop_covered <- ci_rubin_prop_covered + as.numeric(dataSets[[i]]$ci_rubin_covered)
      coverage_min <- min(coverage_min,ci_rubin_prop_covered)
      coverage_max <- max(coverage_max,ci_rubin_prop_covered)
    }
    ci_rubin_prop_covered <- ci_rubin_prop_covered/nMatches

    return(list(est=mean_est,
                se=se_total,
                ci_lb=mean_est-1.96*se_total,
                ci_ub=mean_est+1.96*se_total,
                resid_sd=NA,
                coverage=ci_rubin_prop_covered,
                coverage_min = coverage_min,
                coverage_max = coverage_max))
    }
  return(f)
}