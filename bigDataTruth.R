function(instructions,
         simulators){
  #Set up matrix to hold truths
  truths <- data.frame(dag=unique(names(simulators)))
  truths$TrueBigPop <- NA
  truths$ConfoundingStrength <- NA

  # What is the true causal effect in a large data set?
  for(dagIndex in 1:nrow(truths)){
    dagName <- truths[dagIndex,"dag"]
    bigData <- simulators[[dagName]]$simulate(instructions$truth$n)
    bigData_v0 <- simulators[[dagName]]$simulate(instructions$truth$n,
                                                 interventions=list(V=0),
                                                 uMx=bigData$u)
    bigData_v1 <- simulators[[dagName]]$simulate(instructions$truth$n,
                                                 interventions=list(V=1),
                                                 uMx=bigData$u)
    truth <- mean(bigData_v1$d$Y) - mean(bigData_v0$d$Y)
    truths[dagIndex,"TrueBigPop"] <- truth

    #Calculate true amount of confounding (linear dgs assumed)
    ## NB: This will need modification for different true DGSs
    if("C" %in% colnames(bigData$d)){
      mod_adj <- lm(Y~C+V,bigData$d)
      mod_unadj <- lm(Y~V,bigData$d)
      truths[dagIndex,"ConfoundingStrength"] <-  mod_unadj$coefficients[["V"]] - mod_adj$coefficients[["V"]]
    }
  }
  return(truths)
}