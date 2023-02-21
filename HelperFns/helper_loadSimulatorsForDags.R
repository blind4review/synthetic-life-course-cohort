function(options, msg){
  f <- function(dags, simFns){
    simulators <- list()
    for(dagName in names(dags)){
      msg$print(paste0("Simulation order for: ", dagName))
      simulators[[dagName]] <- dget("simulator.R")(options$simulation,
                                                   dags[[dagName]],
                                                   simFns,
                                                   msg)
    }
    return(simulators)
  }
  return(f)
}
