function(d,
         varName,
         parents,
         simOptions,
         u=NA){

  #Generate noise variable
  if(is.na(u)){
    u <- rnorm(nrow(d),0,sd=simOptions$coefs$U)
  }
  v <- u

  #Generate effects of parents
  for(parent in parents){
    v <- v + simOptions$coefs[[parent]]*d[,parent]
  }

  #Add ixns here
  if("ixnCoefs" %in% names(simOptions)){
    for(ixnCoef in names(simOptions$ixnCoefs)){
      ixnCoefs <- simOptions$ixnCoefs[[ixnCoef]]
      parent1 <- ixnCoefs$var1
      parent2 <- ixnCoefs$var2
      v <- v + ixnCoefs$size*d[,parent1]*d[,parent2]
    }
  }

  return(list(v=v,u=u))
}