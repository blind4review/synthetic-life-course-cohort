function(distOptions=NULL){
  distOptions <- distOptions

  f <- function(cohorts){
    #Calculate distances
    distanceMx <- matrix(0,
                         nrow=nrow(cohorts$younger),
                         ncol=nrow(cohorts$older))

    weights <- list()
    distVars <- distOptions$vars

    # Use R-squared to calculate weights
    if(distOptions$distanceMetric=="rsq_weighted"){
      for(distVar in distVars){
        aFormula <- as.formula(paste0("V~",distVar))
        Amodel <- lm(aFormula, data=cohorts$younger)
        weights[[distVar]] <- summary(Amodel)$r.squared
      }
    }

    #Loop through young people and calculate their distances to older
    for(i in 1:nrow(cohorts$younger)){
      youngerVals <- cohorts$younger[i,distVars,drop=FALSE]
      var_dists <- cohorts$older[,distVars,drop=FALSE]

      if(distOptions$distanceMetric=="unweighted"){
        for(var in distVars){
          var_dists[,var] <- (var_dists[,var]-youngerVals[1,var])^2
        }
      }else if(distOptions$distanceMetric=="rsq_weighted"){
        for(var in distVars){
          weight <- weights[[var]]
          var_dists[,var] <- (1/weight)*(var_dists[,var]-youngerVals[1,var])^2
        }
      }else{
        stop("Invalid option for 'distOptions$distanceMetric'")
      }
      distances <- apply(var_dists,1,sum)

      distanceMx[i,] <- distances
    }
    return(list(comment="Rows of distanceMx are younger people, cols are older people",
                distanceMx=distanceMx,
                weights=weights))
  }
  return(f)
}