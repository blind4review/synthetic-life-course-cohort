function(combOptions=NULL){
  #Using prob matching number one (frome email to Katrina)
  #  with weight put into the analysis along with number of matches
  combOptions <- combOptions

  f <- function(cohorts,
                distanceResults){
    # Select a fixed number "combOptions$nMatches" matches for every referent,
    # with matches chosen probabilistically:
    #  The selected person indexed by "j" has (1/distance_ij)/(sum_ij(1/distance_j))
    #   probability of being selected.
    distanceMx  <- distanceResults$distanceMx
    weightsMx <- 1/distanceMx
    sumWeights <- apply(weightsMx,2,sum)
    probsMx <- t(t(weightsMx)/sumWeights)

    # Select with replacement
    #  Matches will contain all of the match indices across all bootstraps
    #  (in column groups of size combOptions$nMatches)
    matches <- t(apply(probsMx,2,function(y){
      sample(1:nrow(probsMx),
             combOptions$nMatches*combOptions$nBootstraps,
             replace=TRUE,
             prob=y)}
    ))

    if(combOptions$nMatches==1){
      matches <- t(data.frame(matches))
    }

    return(list(comment_matches=paste0("Rows of 'matches' are older people, columns are their younger matches, in ",
                         combOptions$nBootstraps," bootstrap repetition sets of ",combOptions$nMatches),
                comment_weightsMx=paste0("Rows of 'weightsMx' are younger people, columns are older people"),
                matches=matches,
                weightsMx=weightsMx))
  }
  return(f)
}