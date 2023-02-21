function(dirs){
  f<-function(instructions){
    #Figure out what unique specifications need to be used to load
    toLoad <- c()
    for(dag in instructions$dags){
      combSpecs <- dag$combining
      for(combSpec in combSpecs){
        distSpecName <- instructions$combining[[combSpec]]$options$distance
        toLoad <- c(toLoad,distSpecName)
      }
    }

    #Load distance function templates, instantiated with their options, to create the list of distance fns
    distFns <- list()
    for(distanceSpecName in toLoad){
      if(!(distanceSpecName %in% names(instructions$distances))){
        stop(paste0("Distance spec '",distanceSpecName,"' does not exist"))
      }
      distSpec <- instructions$distances[[distanceSpecName]]
      distFnName <- distSpec$distanceFn
      distSpecOptions <- distSpec$options
      distFns[[distanceSpecName]] <- dget(file.path(dirs$DistanceFns,paste0(distFnName,".R")))(distSpecOptions)
    }
    return(distFns)
  }
  return(f)
}