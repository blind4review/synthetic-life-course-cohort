function(dirs){
  f <- function(instructions){
    dags <- list()
    for(dagName in names(instructions$dags)){
      dags[[dagName]] <- dget(file.path(dirs$DAGs,paste0(dagName,".R")))
    }
    return(dags)
  }
  return(f)
}