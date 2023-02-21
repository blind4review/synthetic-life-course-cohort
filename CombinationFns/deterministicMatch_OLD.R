function(combOptions=NULL){
  combOptions <- combOptions

  f <- function(cohorts,
                distanceMxList,
                analysisFunction){

    distanceMx  <- distanceMxList$distanceMx

    #Select best matches
    bestMatches <- as.data.frame(matrix(nrow=nrow(cohorts$younger),
                                        ncol=combOptions$nMatches),
                                 drop=FALSE)
    colnames(bestMatches) <- paste0("older", 1:combOptions$nMatches)
    rownames(bestMatches) <- paste0("younger", 1:nrow(bestMatches))


    matches <- t(apply(distanceMx,1,function(x){order(x)}))
    lookup <- data.frame(younger=numeric(),
                         older=numeric())
    for(j in 1:combOptions$nMatches){
      matches_j <- matches[,j]
      bestMatches[,j] <- matches_j
      lookup <- rbind(lookup,
                      data.frame(younger_ID=1:nrow(bestMatches),
                                 older_ID=matches_j))
    }

    cohorts$younger$younger_ID <- 1:nrow(cohorts$younger)
    cohorts$older$ID <- 1:nrow(cohorts$older)

    colnames(cohorts$older) <- paste0("older_",colnames(cohorts$older))

    d <- cohorts$younger %>%
      left_join(lookup) %>%
      left_join(cohorts$older) %>%
      dplyr::rename(Y=older_Y)

    return(list(d=d,
                bestMatches=bestMatches,
                distanceMx=distanceMx))
  }
  return(f)
}