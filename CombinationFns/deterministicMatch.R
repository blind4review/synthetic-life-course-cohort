function(combOptions=NULL){
  combOptions <- combOptions

  f <- function(cohorts,
                distanceResults){
    #"Rows of 'distanceMx' are younger people, columns are older people"
    distanceMx  <- distanceResults$distanceMx

    #Select best matches
    matches <- t(apply(distanceMx,2,function(x){order(x)}))
    bestMatches <- matches[,1:combOptions$nMatches,drop=FALSE]

    return(list(comment_matches=paste0("Rows of 'matches' are older people, columns are their younger matches (nMatches=", combOptions$nMatches,")"),
                comment_weightsMx=paste0("Rows of 'weightsMx' are younger people, columns are older people"),
                matches=bestMatches,
                weightsMx=1/distanceMx #Weights not used in this script but are needed for some analyses so we return them
                ))
  }
  return(f)
}