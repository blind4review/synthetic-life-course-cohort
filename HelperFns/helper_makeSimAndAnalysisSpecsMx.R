function(){
  f <- function(instructions){
    #Set up data frame to hold estimates
    estCols <- data.frame(dag=character(),
                          measurement=character(),
                          combining=character(),
                          analysis=character())

    #Loop over each DAG, measurement function, and measurement
    #  function specified in the dag to set up results matrix
    for(dagName in names(instructions$dags)){
      dag <- instructions$dags[[dagName]]
      for(measurementName in names(dag$measurement)){
        dagQualFns <- instructions$dags[[dagName]]$matchQuality
        estCols <- rbind(estCols,
                         expand.grid(dag=dagName,
                                     measurement=measurementName,
                                     combining=dag$combining,
                                     analysis=dag$measurement[[measurementName]],
                                     stringsAsFactors=FALSE))
      }
    }
    return(estCols)
  }
  return(f)
}