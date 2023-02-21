function(dirs){
  f<-function(dags){
    simFns <- list()
    for(dag in names(dags)){
      for(var in names(dags[[dag]])){
        simFnName <- dags[[dag]][[var]]$simFn
        if(!(simFnName %in% names(simFns))){
          simFns[[simFnName]] <- dget(file.path(dirs$SimFns,paste0(simFnName,".R")))
        }
      }
    }
    return(simFns)
  }
  return(f)
}