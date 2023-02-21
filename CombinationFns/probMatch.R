function(combOptions=NULL){
  #Using prob matching number one (frome email to Katrina)
  #  with weight put into the analysis along with number of matches
  combOptions <- combOptions

  f <- function(cohorts,
                distanceMxList){

    distanceMx  <- distanceMxList$distanceMx

    #Select best matches
    bestMatches <- as.data.frame(matrix(nrow=nrow(cohorts$younger),
                                        ncol=combOptions$nMatches),
                                 drop=FALSE)
    colnames(bestMatches) <- paste0("older", 1:combOptions$nMatches)
    rownames(bestMatches) <- paste0("younger", 1:nrow(bestMatches))

    weightsMx <- 1/distanceMx
    sumWeights <- apply(weightsMx,1,sum)
    probsMx <- weightsMx/sumWeights

    #Matches will contain all of the match indices across all bootstraps
    # (in column groups of size combOptions$nMatches)
    matches <- t(apply(probsMx,1,function(y){
      sample(1:ncol(probsMx),combOptions$nMatches*combOptions$nBootstraps,
             replace=TRUE,
             prob=y)}
    ))

    return(list(matches,
                weightsMx=weightsMx))
  }
  return(f)
}